open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.map Int32.Parse

let getFuelRequired num = num / 3 - 2

let rec getFuelRequiredIncludingFuel required num =
    let fuel = num / 3 - 2

    if fuel < 1 then
        required
    else
        getFuelRequiredIncludingFuel (required + fuel) fuel

let data = loadData ()

data
|> Array.map getFuelRequired
|> Array.sum
|> printfn "Part 1: %d"


data
|> Array.map (getFuelRequiredIncludingFuel 0)
|> Array.sum
|> printfn "Part 2: %d"
