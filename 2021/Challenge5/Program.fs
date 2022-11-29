open System

type Coord = { x : int; y : int}

let parseCoord (text : string) =
  text.Split(",")
  |> Array.map Int32.Parse
  |> function
     | [| x; y |] -> {x = x; y = y}
     | _ -> failwith "Invalid coords"

let parseCoords (text : string) =
  text.Split(" -> ")
  |> Array.map parseCoord
  |> function
     | [| a; b |] -> (a,b)
     | _ -> failwith "Invalid coords"

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseCoords
  |> Array.toList

let getMax getValue data =
  let greatest =
    data
    |> List.collect (fun (a,b) -> [a;b])
    |> List.maxBy getValue
  getValue greatest

let getOrderedNumbers a b =
  if a > b then
    (b, a)
  else
    (a, b)

let addVertical (grid : int[][]) x y1 y2 =
  let (y1, y2) = getOrderedNumbers y1 y2
  for y in y1..y2 do
    grid[y][x] <- grid[y][x] + 1
  grid

let addHorizontal (grid : int[][]) y x1 x2 =
  let (x1, x2) = getOrderedNumbers x1 x2
  for x in x1..x2 do
    grid[y][x] <- grid[y][x] + 1
  grid

let addDiagonal (grid : int[][]) x1 x2 y1 y2 =
  let (nx1, nx2) = getOrderedNumbers x1 x2
  let (ny1, ny2) = getOrderedNumbers y1 y2
  let xSwapped = nx1 <> x1
  let ySwapped = ny1 <> y1
  let swap swapped sequence =
    if swapped
    then sequence |> Seq.rev
    else sequence
  let xSeq = seq { nx1..nx2 } |> swap xSwapped
  let ySeq = seq { ny1..ny2 } |> swap ySwapped
  let zipped = Seq.zip xSeq ySeq
  for (x,y) in zipped do
    grid[y][x] <- grid[y][x] + 1
  grid

let addHorizontalAndVerticalLinesToGrid (grid : int[][]) line =
  match line with
  | ({x = x1; y = y1}, {x = x2; y = y2}) when x1 = x2 ->
    addVertical grid x1 y1 y2 |> Some
  | ({x = x1; y = y1}, {x = x2; y = y2}) when y1 = y2->
    addHorizontal grid y1 x1 x2 |> Some
  | _ -> None

let addAllLinesToGrid (grid : int[][]) line =
  match line with
  | ({x = x1; y = y1}, {x = x2; y = y2}) when x1 = x2 ->
    addVertical grid x1 y1 y2 |> Some
  | ({x = x1; y = y1}, {x = x2; y = y2}) when y1 = y2->
    addHorizontal grid y1 x1 x2 |> Some
  | ({x = x1; y = y1}, {x = x2; y = y2}) ->
    addDiagonal grid x1 x2 y1 y2 |> Some

let rec populateGrid addLine lines (grid : int[][]) =
  match lines with
  | [] -> grid
  | line :: tail ->
    match addLine grid line with
    | Some updated -> populateGrid addLine tail updated
    | None -> populateGrid addLine tail grid

let data = loadData()
let maxX = (getMax (fun {x = x; y = y} -> x) data) + 1
let maxY = (getMax (fun {x = x; y = y} -> y) data) + 1

let createGrid () =
  Array.init maxY (fun _ -> Array.zeroCreate<int> maxX)

let gridNonDiag = createGrid()

let populatedNonDiag = populateGrid addHorizontalAndVerticalLinesToGrid data gridNonDiag
let allCountsNonDiag = populatedNonDiag |> Array.collect id

let greaterThan1CountNonDiag =
  allCountsNonDiag
  |> Array.filter (fun v -> v > 1)
  |> Array.length

printfn "Count non-diag: %d" greaterThan1CountNonDiag

let grid = createGrid()

let populatedAll = populateGrid addAllLinesToGrid data grid
let allCounts = populatedAll |> Array.collect id

let greaterThan1Count =
  allCounts
  |> Array.filter (fun v -> v > 1)
  |> Array.length

printfn "Count all: %d" greaterThan1Count