type BracketType =
  | Parenthesis
  | Square
  | Angle
  | Brace

type Bracket =
  | Open of BracketType
  | Close of BracketType

let parseBracket =
  function
  | '(' -> Open Parenthesis
  | ')' -> Close Parenthesis
  | '[' -> Open Square
  | ']' -> Close Square
  | '<' -> Open Angle
  | '>' -> Close Angle
  | '{' -> Open Brace
  | '}' -> Close Brace
  | _ -> failwith "Invalid bracket"

let parseLine (line : string) =
  line.ToCharArray()
  |> Array.map parseBracket
  |> Array.toList

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseLine
  |> Array.toList

let getBracketPoints =
  function
  | Parenthesis -> 3
  | Square ->  57
  | Brace -> 1197
  | Angle -> 25137

let getBracketCompletionPoints =
  function
  | Parenthesis -> 1L
  | Square ->  2L
  | Brace -> 3L
  | Angle -> 4L

let rec getBracketCompletionScore score brackets =
  match brackets with
  | [] -> score
  | bracket :: tail ->
    let points = getBracketCompletionPoints bracket
    getBracketCompletionScore (score * 5L + points) tail

let rec getCorruptedBracket openBrackets brackets =
  match (openBrackets, brackets) with
  | (_, []) -> None
  | (openType :: _, Close bracketType :: _)
    when bracketType <> openType -> Some bracketType
  | (_ :: remainingOpen, Close _ :: tail) ->
    getCorruptedBracket remainingOpen tail
  | ([], Close _ :: tail) ->
    getCorruptedBracket [] tail
  | (_, Open openType :: tail) ->
    getCorruptedBracket (openType :: openBrackets) tail

let rec getCompletionBrackets openBrackets brackets =
  match openBrackets with
  | [] -> brackets |> List.rev
  | bracketType :: tail ->
    getCompletionBrackets tail (bracketType :: brackets)

let rec completeBrackets openBrackets brackets =
  match (openBrackets, brackets) with
  | (_, []) ->
    let completionBrackets = getCompletionBrackets openBrackets []
    completionBrackets
  | (_ :: remainingOpen, Close _ :: tail) ->
    completeBrackets remainingOpen tail
  | ([], Close _ :: tail) ->
    completeBrackets [] tail
  | (_, Open openType :: tail) ->
    completeBrackets (openType :: openBrackets) tail

let data = loadData()
let corrupted = data |> List.choose (getCorruptedBracket [])
let result = corrupted |> List.map getBracketPoints |> List.sum

printfn "Part 1: %d" result

let incomplete = data |> List.filter ((getCorruptedBracket []) >> Option.isNone)
let completionBrackets = incomplete |> List.map (completeBrackets [])
let completionBracketScores = completionBrackets |> List.map (getBracketCompletionScore 0)

let middleScore =
  completionBracketScores
  |> List.sort
  |> List.skip (List.length completionBracketScores / 2)
  |> List.head

printfn "Part 2: %d" middleScore