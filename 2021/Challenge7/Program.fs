open System

let loadData () =
  let firstLine = System.IO.File.ReadAllLines("./input.txt") |> Array.head
  firstLine.Split(",")
  |> Array.map Int32.Parse
  |> Array.toList

let findMedian (items : int list) =
  let ordered = List.sort items
  let count = List.length ordered
  let toTake = if count % 2 = 0 then 1 else 2
  let mid = (count - 1) / 2
  ordered
  |> List.skip mid
  |> List.take toTake
  |> List.sum
  |> (fun sum -> sum / toTake)

let rec getFuelCost position cost items =
  match items with
  | [] -> cost
  | item :: tail ->
    let newCost = (Math.Abs : int -> int) (item - position)
    getFuelCost position (cost + newCost) tail

let rec getPartTwoFuelCost position cost items =
  let getCost distance =
    seq { 0..distance }
    |> Seq.sum
  match items with
  | [] -> cost
  | item :: tail ->
    let newCost = (Math.Abs : int -> int) (item - position) |> getCost
    getPartTwoFuelCost position (cost + newCost) tail

let data = loadData()
let median = findMedian data
getFuelCost median 0 data
|> printfn "Part 1: %d"

let min = List.min data
let max = List.max data
seq { min..max}
|> Seq.map (fun i -> getPartTwoFuelCost i 0 data)
|> Seq.min
|> printfn "Part 2: %d"
