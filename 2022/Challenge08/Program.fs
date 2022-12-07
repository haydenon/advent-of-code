open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text
    |> List.ofArray


let data = loadData ()

data
|> printfn "%A"
