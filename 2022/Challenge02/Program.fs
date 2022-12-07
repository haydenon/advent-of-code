open System

type Action =
    | Rock
    | Paper
    | Scissors

let parseOpponent action =
    match action with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith "Invalid input"


let parseSelfAction action =
    match action with
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> failwith "Invalid input"

let parseSelfResult opp result =
    match result with
    | "X" ->
        match opp with
        | Rock -> Scissors
        | Paper -> Rock
        | Scissors -> Paper
    | "Y" -> opp
    | "Z" ->
        match opp with
        | Scissors -> Rock
        | Rock -> Paper
        | Paper -> Scissors
    | _ -> failwith "Invalid input"

let getMatchPoints (opp, self) =
    match (opp, self) with
    | (a, b) when a = b -> 3
    | (Rock, Scissors) -> 0
    | (Rock, Paper) -> 6
    | (Paper, Rock) -> 0
    | (Paper, Scissors) -> 6
    | (Scissors, Paper) -> 0
    | (Scissors, Rock) -> 6
    | _ -> failwith "Invalid inputs"

let getActionPoint =
    function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let getPointsForRow (opp, self) =
    getMatchPoints (opp, self) + getActionPoint (self)

let parseRow (row: string) =
    match row.Split(" ") with
    | [| opp; self |] ->
        let opp = parseOpponent opp
        (opp, parseSelfAction self), (opp, parseSelfResult opp self)
    | _ -> failwith "Invalid input"

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> List.ofArray |> List.map parseRow

let data = loadData ()

data
|> List.map fst
|> List.sumBy getPointsForRow
|> printfn "Part 1: %d"

data
|> List.map snd
|> List.sumBy getPointsForRow
|> printfn "Part 2: %d"
