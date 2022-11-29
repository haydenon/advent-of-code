open System

let parseLine (line : string) =
  line.ToCharArray()
  |> Array.map (Char.ToString >> Int32.Parse)

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseLine

let deltas = [
  (0, -1)
  (1, 0)
  (0, 1)
  (-1, 0)
]

let getValue (grid : int[][]) (x,y) = grid[y][x]

let inBounds (maxX, maxY) (x, y) =
    x >= 0 && y >= 0 && x < maxX && y < maxY

let getAdjacent size x y =
  deltas
  |> List.map (fun (dx, dy) -> (x + dx, y + dy))
  |> List.filter (inBounds size)


let rec findLowPoints (grid : int[][]) size points x y =
  let (maxX, maxY) = size
  match (x, y) with
  | _ when y >= maxY -> points
  | _ when x >= maxX -> findLowPoints grid size points 0 (y + 1)
  | _ ->
    let value = grid[y][x]
    let isLower (x, y) =
      grid[y][x] > value
    let adjacent = getAdjacent size x y
    let updated = if adjacent |> List.forall isLower then (x,y) :: points else points
    findLowPoints grid size updated (x + 1) y

let rec findBasin (grid : int[][]) size inBasin pointsToCheck =
  let notInList list point =
    list
    |> List.contains point
    |> not
  match pointsToCheck with
  | [] -> inBasin
  | (x,y) :: tail ->
    let inBasin = (x,y) :: inBasin
    let adjacent =
      getAdjacent size x y
      |> List.filter (getValue grid >> ((<>)9))
      |> List.filter (notInList inBasin)
      |> List.filter (notInList tail)
    List.append tail adjacent
    |> findBasin grid size inBasin

let calculateRiskScore (grid : int[][]) points =
  points
  |> List.map (getValue grid >> ((+)1))
  |> List.sum

let data = loadData()
let size = (Array.length data[0], Array.length data)
let lowPoints = findLowPoints data size [] 0 0
let riskScore = calculateRiskScore data lowPoints

printfn "Part 1: %d" riskScore

let basinsValues =
  lowPoints
  |> List.map (fun lowPoint -> findBasin data size [] [lowPoint])
  |> List.map (List.map (getValue data))

let threeLargest =
  basinsValues
  |> List.map (fun values -> List.length values)
  |> List.sortDescending
  |> List.take 3

let result = threeLargest |> List.reduce (*)

printfn "Part 2: %d" result
