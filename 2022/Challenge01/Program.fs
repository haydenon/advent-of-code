open System

let rec getLists (elveCalories: int list list) (current: int list) (lines: string list) =
    match lines with
    | [] ->
        match current with
        | [] -> elveCalories
        | _ -> current :: elveCalories
    | line :: rest ->
        if String.IsNullOrWhiteSpace line then
            getLists (current :: elveCalories) [] rest
        else
            let calories = Int32.Parse(line)
            getLists elveCalories (calories :: current) rest

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> List.ofArray |> getLists [] []

let sumElveCalories (list: int list list) = list |> List.map List.sum

let getMax (list: int list list) = list |> sumElveCalories |> List.max

let getTopThree (list: int list list) =
    list
    |> sumElveCalories
    |> List.sortDescending
    |> List.take 3
    |> List.sum

let data = loadData ()
data |> getMax |> printfn "Part 1: %d"

data |> getTopThree |> printfn "Part 2: %d"