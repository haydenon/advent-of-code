open System

type Action =
    | Rock
    | Paper
    | Scissors

let parseRow (row: string) =
    let middle = row.Length / 2
    row.Substring(0, middle), row.Substring(middle)

let getCommon (a: string, b: string) =
    let setA = a |> Seq.toList |> Set.ofList
    let setB = b |> Seq.toList |> Set.ofList
    Set.intersect setA setB |> Set.toList |> List.head

let getCommonForThree (a: string, b: string, c: string) =
    let setA = a |> Seq.toList |> Set.ofList
    let setB = b |> Seq.toList |> Set.ofList
    let setC = c |> Seq.toList |> Set.ofList

    Set.intersect setA setB
    |> Set.intersect setC
    |> Set.toList
    |> List.head

let chunk size items =
    items
    |> List.mapi (fun i x -> (i / size, x))
    |> Seq.groupBy fst
    |> Seq.map snd
    |> Seq.map (Seq.map snd >> Seq.toList)
    |> Seq.toList

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    (text |> List.ofArray |> List.map parseRow, chunk 3 (text |> List.ofArray))

let data = loadData ()

let getPoints (ch: char) =
    if (int ch) >= (int 'a') then
        (int ch) - (int 'a') + 1
    else
        (int ch - int 'A') + 27


fst data
|> List.map getCommon
|> List.map getPoints
|> List.sum
|> printfn "Part 1: %A"

snd data
|> List.map (function
    | [ a; b; c ] -> getCommonForThree (a, b, c)
    | _ -> failwith "Invalid input")
|> List.map getPoints
|> List.sum
|> printfn "Part 2: %d"
