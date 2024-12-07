open System
open System.Linq

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text
    |> Array.map (fun line ->
        let parts = line.Split(": ")

        parts[0] |> Int64.Parse,
        parts[ 1 ].Split(" ")
        |> Array.map Int64.Parse
        |> Array.toList)
    |> Array.toList

let data = loadData ()

let rowValid enableConcat ((value, list): int64 * int64 list) =
    let rec getValues (values: int64 list) (list: int64 list) =
        match list with
        | [] -> values
        | head :: rest ->
            let plusTimesValues =
                (values |> List.map (fun i -> i * head))
                |> List.append (values |> List.map (fun i -> i + head))

            let allNewValues =
                if enableConcat then
                    plusTimesValues
                    |> List.append (
                        values
                        |> List.map (fun i -> i.ToString() + head.ToString() |> Int64.Parse)
                    )
                else
                    plusTimesValues

            getValues allNewValues rest

    getValues [ list |> List.head ] (list |> List.skip 1)
    |> List.exists ((=) value)


data
|> List.filter (rowValid false)
|> List.map fst
|> List.sum
|> printfn "Part 1: %d"


data
|> List.filter (rowValid true)
|> List.map fst
|> List.sum
|> printfn "Part 2: %d"
