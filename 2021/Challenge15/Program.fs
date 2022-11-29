open System
open System.Collections.Generic

let parseLine (line : string) =
  line.ToCharArray()
  |> Array.map (Char.ToString >> Int32.Parse)

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseLine

let expandGrid (grid : int[][]) =
  let lengthX, lengthY = (Array.length grid[0], Array.length grid)
  let (newLengthX, newLengthY) = (lengthX * 5, lengthY * 5)
  // let newGrid = Array.init newLengthY (fun _ -> Array.create newLengthX 0)
  // Seq.allPairs (seq{0..(lengthX - 1)}) (seq{0..(lengthY - 1)})
  // |> Seq.iter (fun (x,y) -> newGrid[y][x] <- grid[y][x])

  // Seq.allPairs (seq{0..(newLengthX - 1)}) (seq{0..(newLengthY - 1)})
  // |> Seq.filter (fun (x,y) -> x >= lengthX || y >= lengthY)
  // |> Seq.iter
  let fillGrid start =
   let gridLine = Array.init lengthY (fun _ -> Array.create newLengthX 0)
   Seq.allPairs (seq{0..(lengthX - 1)}) (seq{0..(lengthY - 1)})
   |> Seq.iter (fun (x,y) -> gridLine[y][x] <- ((grid[y][x] + start - 1) % 9) + 1)

   Seq.allPairs (seq{lengthX..(newLengthY - 1)}) (seq{0..(lengthY - 1)})
   |> Seq.iter (fun (x,y) -> gridLine[y][x] <- ((gridLine[y][x - lengthX]) % 9) + 1)
   gridLine

  seq {0..4}
  |> Seq.map fillGrid
  |> Seq.toArray
  |> Array.collect id

let deltas = [
  (0, -1)
  (1, 0)
  (0, 1)
  (-1, 0)
]

let getValue (grid : int[][]) (x,y) =
  grid[y][x]

let inBounds (maxX, maxY) (x, y) =
    x >= 0 && y >= 0 && x < maxX && y < maxY

let getAdjacent size (x, y) =
  deltas
  |> List.map (fun (dx, dy) -> (x + dx, y + dy))
  |> List.filter (inBounds size)

let addCostsToMap pathCost points =
  let shouldAddCostToMap pathCost (point, cost) =
    if pathCost |> Map.containsKey point && pathCost[point] <= cost
    then false
    else
      true
  let addCostToMap pathCost (point, cost) =
      pathCost |> Map.add point cost
  points
  |> List.filter (shouldAddCostToMap pathCost)
  |> List.fold addCostToMap pathCost

let findShortestPathCost (grid : int[][]) =
  let size = (Array.length grid[0], Array.length grid)
  let sizeX, sizeY = size
  let target = (sizeX - 1, sizeY - 1)
  let queue = PriorityQueue<(int * int), int>()
  queue.Enqueue((0,0), 0)
  let pathCost = [(0,0), 0] |> Map.ofList
  let visited = Set.empty
  let rec findShortest (pathCost : Map<(int * int), int>) visited (queue : PriorityQueue<(int * int), int>) =
    match queue.Count with
    | 0 |  _ when visited |> Set.contains target -> pathCost[target]
    | _ ->
      let point = queue.Dequeue()
      if visited |> Set.contains point
      then findShortest pathCost visited queue
      else
        let cost = pathCost[point]
        let adjacentWithCost =
          getAdjacent size point
          |> List.filter (fun p -> visited |> Set.contains p |> not)
          |> List.map (fun p -> (p, (getValue grid p) + cost))
        let newPathCost = addCostsToMap pathCost adjacentWithCost
        let newVisisted = visited |> Set.add point
        adjacentWithCost
        |> List.iter(fun (point, cost) -> queue.Enqueue(point, cost))
        findShortest newPathCost newVisisted queue
  findShortest pathCost visited queue

let data = loadData()
findShortestPathCost data
|> printfn "Part 1: %d"

let largerGrid = expandGrid data
findShortestPathCost largerGrid
|> printfn "Part 2: %d"

