open System

let rec parseInitialState num stacks (stateRows: string list) =
    let addItem (row: string) (stacks: char list array) num =
        let idx = num * 4 + 1

        if row.Length > idx && row.[idx] <> ' ' then
            stacks.[num] <- row.[idx] :: stacks.[num]
        else
            ()

    match stateRows with
    | [] -> stacks
    | row :: rest ->
        seq { 0..num } |> Seq.iter (addItem row stacks)
        parseInitialState num stacks rest

let rec getInitialStateRows stateRows (rows: string list) =
    match rows with
    | row :: rest ->
        if row.StartsWith " 1" then
            (row.Split(" ").Length / 3), stateRows, rest
        else
            getInitialStateRows (row :: stateRows) rest
    | _ -> failwith "Invalid input"

type Instruction = { Count: int; From: int; To: int }

let rec parseInstructions instructions (rows: string list) =
    match rows with
    | row :: rest ->
        if String.IsNullOrWhiteSpace row then
            parseInstructions instructions rest
        else
            let parts = row.Split(" ")

            let instructions =
                { Count = Int32.Parse parts[1]
                  From = Int32.Parse parts[3] - 1
                  To = Int32.Parse parts[5] - 1 }
                :: instructions

            parseInstructions instructions rest
    | [] -> instructions

let rec run reverse (stacks: char list array) instructions =
    match instructions with
    | [] -> stacks
    | { Count = count; From = from; To = too } :: rest ->
        let toMove =
            stacks[from]
            |> List.take count
            |> if reverse then List.rev else id

        stacks[from] <- stacks[from] |> List.skip count

        stacks[too] <- List.append toMove stacks[too]

        run reverse stacks rest

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    let num, stateRows, instructions = text |> List.ofArray |> getInitialStateRows []

    let stacks: char list array =
        seq { 0..num }
        |> Seq.map (fun _ -> List.empty<char>)
        |> Seq.toArray

    parseInitialState num stacks stateRows, parseInstructions [] instructions |> List.rev

let stack, instructions = loadData ()

let outputTop (stacks: char list array) =
    let mapped: string [] =
        stacks
        |> Array.map (function
            | ch :: _ -> string ch
            | [] -> String.Empty)

    String.Join("", mapped)

let stack1 = stack |> Array.map id

run true stack1 instructions
|> outputTop
|> printfn "Part 1: %s"


let stack2 = stack |> Array.map id

run false stack2 instructions
|> outputTop
|> printfn "Part 2: %s"
