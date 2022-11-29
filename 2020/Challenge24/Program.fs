open System

type Direction =
  | East
  | SouthEast
  | SouthWest
  | West
  | NorthWest
  | NorthEast

let (|StartsWith|_|) (start : string) (str : string) = if str.StartsWith start then Some() else None

let rec parseLine directions (line : string) =
  if String.IsNullOrWhiteSpace line
  then List.rev directions
  else
    let (direction, chars) =
      match line with
      | StartsWith "e" -> (East, 1)
      | StartsWith "se" -> (SouthEast, 2)
      | StartsWith "sw" -> (SouthWest, 2)
      | StartsWith "w" -> (West, 1)
      | StartsWith "nw" -> (NorthWest, 2)
      | StartsWith "ne" -> (NorthEast, 2)
      | _ -> failwith "Invalid input"
    parseLine (direction :: directions) (line.Substring(chars))

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map (parseLine [])
  |> Array.toList

let getMovement (x,y) =
  function
  | East      -> (x - 2, y)
  | West      -> (x + 2, y)
  | SouthWest -> (x + 1, y + 1)
  | SouthEast -> (x - 1, y + 1)
  | NorthWest -> (x + 1, y - 1)
  | NorthEast -> (x - 1, y - 1)

let updateState state coords =
  if Map.containsKey coords state
  then Map.remove coords state
  else Map.add coords () state

let rec followSteps coords state directions =
  match directions with
  | [] -> (updateState state coords)
  | direction :: tail ->
    let newCoords = getMovement coords direction
    followSteps newCoords state tail

let stateAtCoords state coords =
  Map.containsKey coords state

let getAdjacent (x,y) =
  [(x - 2, y);(x + 2, y);(x + 1, y + 1);(x - 1, y + 1);(x + 1, y - 1);(x - 1, y - 1);]

let getSelfAndAdjacent coords =
  coords :: getAdjacent coords

let getToCheck state =
  state
  |> Map.toList
  |> List.map fst
  |> List.collect getSelfAndAdjacent
  |> Set.ofList
  |> Set.toList

let countAdjacent state coords checkState =
  let adj = getAdjacent coords
  adj
  |> List.filter ((stateAtCoords state) >> ((=) checkState))
  |> List.length

let rec applyChanges state changes =
  match changes with
  | [] -> state
  | (coords, add) :: tail ->
    let upd =
      if add
      then Map.add coords () state
      else Map.remove coords state
    applyChanges upd tail

let rec runDays days currDay state =
  if currDay > days
  then state
  else
    let toCheck = getToCheck state
    let getChange changes coords =
      let isBlack = stateAtCoords state coords
      let adjBlack = countAdjacent state coords true
      match (isBlack, adjBlack) with
      | (false, 2) -> (coords, true) :: changes
      | (true, num) when num = 0 || num > 2 -> (coords, false) :: changes
      | _ -> changes
    let changes = List.fold getChange [] toCheck
    let upd = applyChanges state changes
    runDays days (currDay + 1) upd

let countBlack state =
  state
  |> Map.toList
  |> List.length

[<EntryPoint>]
let main _ =
  let data = loadData()

  let finishState = data |> List.fold (followSteps (0,0)) Map.empty
  finishState
  |> countBlack
  |> printfn "%A black tiles were left up after being identified"

  runDays 100 1 finishState
  |> Map.toList
  |> List.length
  |> printfn "%d"
  0