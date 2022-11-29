open System

type SeatState =
  | Floor
  | Occupied
  | Empty

let parseRow (row : string) =
  row.ToCharArray()
  |> Array.map (function '.' -> Floor | 'L' -> Empty | _ -> Occupied)

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseRow

let inBounds rows cols r c =
    c > -1 && c < cols && r > -1 && r < rows

let adjacentCount (data : SeatState[][]) rows cols state row col =
  let mutable count = 0
  for r in (row - 1)..(row + 1) do
    for c in (col - 1)..(col + 1) do
      let inc =
        if c = col && r = row then false
        else if inBounds rows cols r c then data.[r].[c] = state
        else false
      if inc then count <- count + 1
  count

let rec runFirstModelChanges adjCount (data : SeatState[][]) =
  let rows = seq { 0..data.Length - 1 }
  let cols = seq { 0..data.[0].Length - 1 }
  let getChange (r, c) =
    match data.[r].[c] with
    | Empty ->
      if (adjCount Occupied r c) = 0
      then Some (r, c, Occupied)
      else None
    | Occupied ->
      if (adjCount Occupied r c) >= 4
      then Some (r, c, Empty)
      else None
    | _ -> None
  let changes =
    Seq.allPairs rows cols
    |> Seq.choose getChange
    |> Seq.toList
  for (r, c, state) in changes do
    data.[r].[c] <- state

  match changes with
  | [] -> data
  | _  -> runFirstModelChanges adjCount data

let calculateAdjacentSeats (data : SeatState[][]) rows cols row col =
  let applyDir (rd, cd) r c =
    (r + rd, c + cd)
  let rec findSeat (r, c) dir  =
    if inBounds rows cols r c then
      match data.[r].[c] with
      | Floor -> findSeat (applyDir dir r c) dir
      | _     -> Some (r, c)
    else None
  let directions = [| (-1,-1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)|]
  directions
  |> Array.choose (fun dir -> findSeat (applyDir dir row col) dir)

let createAdjacentSeatMap (data : SeatState[][]) rows cols =
  let calcAdj = calculateAdjacentSeats data rows cols
  let rows = seq { 0..data.Length - 1 }
  let cols = seq { 0..data.[0].Length - 1 }
  Seq.allPairs rows cols
  |> Seq.filter (fun (r, c) -> data.[r].[c] <> Floor)
  |> Seq.map (fun (r,c) -> ((r,c), calcAdj r c))
  |> Map.ofSeq

type SeatMap = Map<int * int, (int * int)[]>

let rec runSecondModelChanges (seatMap : SeatMap) (data : SeatState[][]) =
  let rows = seq { 0..data.Length - 1 }
  let cols = seq { 0..data.[0].Length - 1 }
  let countState state r c =
    seatMap.[(r,c)]
    |> Array.filter (fun (ar, ac) -> data.[ar].[ac] = state)
    |> Array.length
  let getChange (r, c) =
    match data.[r].[c] with
    | Empty ->
      let cnt = (countState Occupied r c)
      if cnt = 0
      then Some (r, c, Occupied)
      else None
    | Occupied ->
      if (countState Occupied r c) >= 5
      then Some (r, c, Empty)
      else None
    | _ -> None
  let changes =
    Seq.allPairs rows cols
    |> Seq.choose getChange
    |> Seq.toList
  for (r, c, state) in changes do
    data.[r].[c] <- state
  match changes with
  | [] -> data
  | _  -> runSecondModelChanges seatMap data

let printOutput (data : SeatState[][]) =
  let outChar = function Floor -> '.' | Occupied -> '#' | Empty -> 'L'
  for row in data do
    row
    |> Array.map outChar
    |> String
    |> printfn "%s"

let dup (data : SeatState[][]) =
  data
  |> Array.map Array.copy

[<EntryPoint>]
let main _ =
  let data = loadData()
  let rows = Array.length data
  let cols = Array.length data.[0]
  let firstData = dup data
  let adjCount = adjacentCount firstData rows cols
  runFirstModelChanges adjCount firstData
  |> Array.sumBy (fun arr -> arr |> Array.filter (fun state -> state = Occupied) |> Array.length)
  |> printfn "%d seats occupied using first model"

  let sndData = dup data
  let seatMap = createAdjacentSeatMap sndData rows cols
  runSecondModelChanges seatMap sndData
  |> Array.sumBy (fun arr -> arr |> Array.filter (fun state -> state = Occupied) |> Array.length)
  |> printfn "%d seats occupied using second model"
  0