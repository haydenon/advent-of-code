open System;

let loadData () =
  let firstLine = System.IO.File.ReadAllLines("./input.txt") |> Array.head
  firstLine.Split(",")
  |> Array.map Int64.Parse

let getInitialState (seed : int64[]) =
  let getCountForIndex (index : int32) =
    seed
    |> Array.filter (fun n -> n = index)
    |> Array.length
    |> int64
  seq { 0..8 } |> Seq.map getCountForIndex |> Seq.toArray

let updateState (state : int64[]) =
  let getNewState index _ =
    let indexes =
      if index = 6
      then [0;7]
      else [(index + 1) % 9]
    indexes
    |> List.map (fun i -> state[i])
    |> List.sum
  state |> Array.mapi getNewState

let rec runSimulation number state =
  match number with
  | 0 -> state
  | _ ->
    let updated = updateState state
    runSimulation (number - 1) updated

let seed = loadData()
let initialState = getInitialState seed

let part1State = runSimulation 80 initialState
part1State
|> Array.sum
|> printfn "Part 1: %d"
let part2State = runSimulation 256 initialState
part2State
|> Array.sum
|> printfn "Part 2: %d"
