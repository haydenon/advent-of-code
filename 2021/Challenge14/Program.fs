open System

let rec parseInsertionRules rules (lines : string list) =
  match lines with
  | [] -> rules |> Map.ofList
  | line :: tail ->
    let parts = line.Split(" -> ")
    let key = parts[0][0], parts[0][1]
    parseInsertionRules ((key, [parts[1][0]]) :: rules) tail

let loadData () =
  let lines = System.IO.File.ReadAllLines("./input.txt")
  let template = lines[0]
  let insertionRules = parseInsertionRules [] (lines |> Array.skip 2 |> Array.toList)
  (template, insertionRules)

let rec runInsertion (rules : Map<char * char, char list>) steps (polymer : char list) =
  if steps <= 0
  then polymer
  else
    let pairs = polymer |> List.pairwise
    let insert acc (pair : char * char) =
      let newChar = rules[pair] |> List.rev
      let tail = snd pair
      let appended = List.append newChar acc
      tail :: appended
    let newPolymer =
      pairs
      |> List.fold insert [List.head polymer]
      |> List.rev
    runInsertion rules (steps - 1) newPolymer

let getScore (polymer : string) =
  let counts =
    polymer.ToCharArray()
    |> Array.groupBy id
    |> Array.map (snd >> Array.length >> int64)
    |> Array.sort
  let min, max = counts |> Array.head , counts |> Array.last
  max - min

let getCountScore (counts : (char * int64) list) =
  let counts = counts |> List.map snd |> List.sort
  let min, max = counts |> List.head , counts |> List.last
  max - min

let (template, insertionRules) = loadData()

runInsertion insertionRules 10 (template.ToCharArray() |> Array.toList)
|> (fun strs -> String.Join("", strs))
|> getScore
|> printfn "Part 1: %d"

let allChars =
  insertionRules
  |> Map.keys
  |> Seq.collect (fun (c1, c2) -> [c1;c2])
  |> Set.ofSeq
  |> Set.toList

let twentyStepRules =
  List.allPairs allChars allChars
  |> List.map (fun (c1, c2) -> ((c1,c2) , runInsertion insertionRules 20 [c1;c2]))
  |> List.map (fun (chars, output) -> chars,output |> List.skip 1 |> List.take (List.length output - 2))
  |> Map.ofList

let after20Steps =
  runInsertion twentyStepRules 1 (template.ToCharArray() |> Array.toList)

let addToOccurences (occurences : Map<char, int64>) char =
  if Map.containsKey char occurences
  then Map.add char (occurences[char] + 1L) occurences
  else Map.add char 1L occurences

let rec countOccurrences occurences list  =
  match list with
  | [] -> occurences
  | ch :: tail ->
    let occurences = addToOccurences occurences ch
    countOccurrences occurences tail

let after20counts =
    after20Steps
    |> countOccurrences Map.empty

let rulesCounts =
  twentyStepRules
  |> Map.map (fun _ v -> v |> countOccurrences Map.empty)

let pairs = after20Steps |> List.pairwise
let next20Counts =
  pairs
  |> List.map (fun pair -> rulesCounts[pair])

let getTotalCharCount char =
  let add acc (map : Map<char, int64>) =
    (Map.tryFind char map |> Option.defaultValue 0) + acc
  let after20 = add 0 after20counts
  next20Counts
  |> List.fold add after20
allChars
|> List.map (fun c -> c, getTotalCharCount c)
|> getCountScore
|> printfn "Part 2: %d"