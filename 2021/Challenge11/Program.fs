open System

let parseLine (line : string) =
  line.ToCharArray()
  |> Array.map (Char.ToString >> Int32.Parse)

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseLine

let deltas = [
  (-1, -1)
  (0, -1)
  (1, -1)
  (1, 0)
  (1, 1)
  (0, 1)
  (-1, 1)
  (-1, 0)
]

let printGrid (grid : int[][]) =
  for line in grid do
    printfn "%s" (String.Join(" ", line))

let getValue (grid : int[][]) (x,y)=
  grid[y][x]

let rec runFlashes (grid : int[][]) toFlash flashes =
  let inBounds (x, y) =
    let (maxX, maxY) = (Array.length grid[0], Array.length grid)
    x >= 0 && y >= 0 && x < maxX && y < maxY
  let shouldUpdate point =
    inBounds point &&
      getValue grid point <> 0 &&
      (List.contains point toFlash |> not)
  let newFlash point =
    inBounds point &&
      getValue grid point >= 10 &&
      (List.contains point toFlash |> not)
  match toFlash with
  | [] -> (grid, flashes)
  | (x,y) :: tail ->
    grid[y][x] <- 0
    let adjacent =
      deltas
      |> List.map (fun (dx, dy) -> (x + dx, y + dy))
    adjacent
    |> List.filter shouldUpdate
    |> List.iter (fun (x,y) -> grid[y][x] <- grid[y][x] + 1)
    let newFlashes =
      adjacent
      |> List.filter newFlash
    runFlashes grid (List.append tail newFlashes) (flashes + 1)

let rec runSimulation exitCondition (grid : int[][]) days flashes =
  if exitCondition grid days
  then (days, flashes)
  else
    let allCoords =
      Seq.allPairs (seq {0..(Array.length grid - 1)}) (seq {0..(Array.length(grid[0]) - 1)})
      |> Seq.toList
    allCoords
    |> List.iter (fun (x,y) -> grid[y][x] <- grid[y][x] + 1)

    let toFlash = allCoords |> List.filter ((getValue grid) >> ((<=) 10))
    let (grid, newFlashes) = runFlashes grid toFlash 0

    runSimulation exitCondition grid (days + 1) (flashes + newFlashes)

let oneHundredDays _ days =
  days >= 100
let allFlashing (grid : int[][]) _ =
  grid
  |> Array.forall (Array.forall ((=) 0))

let data = loadData()

runSimulation oneHundredDays data 0 0
|> snd
|> printfn "Part 1: %d"

runSimulation allFlashing data 0 0
|> fst
|> printfn "Part 2: %d"
