open System

let parseRow (row: string) =
    row.Split(" ")
    |> Array.map Int32.Parse
    |> List.ofArray

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> List.ofArray |> List.map parseRow

let data = loadData ()

let evaluateRow list =
    let rec checkRow list lessThan =
        match list with
        | head :: next :: rest ->
            let unsafe =
                (lessThan && head > next)
                || (not lessThan && head < next)
                || abs (head - next) < 1
                || abs (head - next) > 3

            if unsafe then
                false
            else
                checkRow (next :: rest) lessThan
        | _ -> true

    match list with
    | first :: second :: rest -> checkRow list true || checkRow list false
    | _ -> failwith "Invalid"

let evaluateRow2 list =
    let checkWithoutIndex (idx: int) =
        evaluateRow (List.append (List.take idx list) (List.skip (idx + 1) list))

    if evaluateRow list then
        true
    else
        list
        |> List.indexed
        |> List.map fst
        |> List.exists checkWithoutIndex

data
|> List.filter evaluateRow
|> List.length
|> printfn "Part1 : %d"

data
|> List.filter evaluateRow2
|> List.length
|> printfn "Part 2: %d"
