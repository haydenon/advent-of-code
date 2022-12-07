open System

let rec parseInitialState num stacks (stateRows: string list) =
    let addItem (row: string) (stacks: char list list) num =
        let idx = num * 4 + 1

        if row.Length > idx && row.[idx] <> ' ' then
            stacks
            |> List.updateAt num (row.[idx] :: stacks[num])
        else
            stacks

    match stateRows with
    | [] -> stacks
    | row :: rest ->
        let stacks =
          seq { 0..num }
          |> Seq.fold (addItem row) stacks
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
    | [] -> instructions |> List.rev

let rec run reverse (stacks: char list list) instructions =
    match instructions with
    | [] -> stacks
    | { Count = count; From = from; To = too } :: rest ->
        let toMove =
            stacks[from]
            |> List.take count
            |> if reverse then List.rev else id

        let newStack =
          stacks
          |> List.updateAt from (stacks[from] |> List.skip count)
          |> List.updateAt too (List.append toMove stacks[too])

        run reverse newStack rest

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    let num, stateRows, instructions = text |> List.ofArray |> getInitialStateRows []

    let stacks: char list list =
        seq { 0..num }
        |> Seq.map (fun _ -> List.empty<char>)
        |> Seq.toList

    parseInitialState num stacks stateRows, parseInstructions [] instructions

let stack, instructions = loadData ()

let outputTop (stacks: char list list) =
    let mapped: string list =
        stacks
        |> List.map (function
            | ch :: _ -> string ch
            | [] -> String.Empty)

    String.Join("", mapped)

run true stack instructions
|> outputTop
|> printfn "Part 1: %s"

run false stack instructions
|> outputTop
|> printfn "Part 2: %s"
