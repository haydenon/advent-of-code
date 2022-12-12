open System
open System.Collections.Generic

let getGrid (lines: string list) =
    let rows = List.length lines
    let cols = List.head lines |> String.length
    let grid = Array.init rows (fun _ -> Array.init cols (fun _ -> ' '))

    seq { 0 .. (rows - 1) }
    |> Seq.iter (fun y ->
        seq { 0 .. (cols - 1) }
        |> Seq.iter (fun x -> grid[y][x] <- lines[ y ].Substring(x, 1) |> char))

    grid


let deltas = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]

let inBounds (maxX, maxY) (x, y) =
    x >= 0 && y >= 0 && x < maxX && y < maxY

let findShortestPathCost isStart (grid: char [] []) =
    let size = (Array.length grid[0], Array.length grid)
    let sizeX, sizeY = size

    let allPairs =
        Seq.allPairs (seq { 0 .. (sizeX - 1) }) (seq { 0 .. (sizeY - 1) })
        |> Seq.toList

    let startPoints =
        allPairs
        |> List.filter (fun (x, y) -> grid[y][x] |> isStart)

    let target =
        allPairs
        |> List.find (fun (x, y) -> grid[y][x] = 'E')

    let getCost (x, y) =
        let (tx, ty) = target
        let xDiff = abs (x - tx)
        let yDiff = abs (y - ty)
        xDiff + yDiff

    let getValue (x, y) = grid[y][x]

    let canStep (x, y) (dx, dy) =
        let mutable value = getValue (x, y)
        let comparison = getValue (dx, dy)
        if value = 'S' then
          value <- 'a'

        if comparison = 'E' then
            value = 'z' || value = 'y'
        elif comparison >= value then
            (int comparison - int value) <= 1
        else
            true

    let getAdjacent size (x, y) =
        deltas
        |> List.map (fun (dx, dy) -> (x + dx, y + dy))
        |> List.filter (inBounds size)
        |> List.filter (canStep (x, y))

    let findForStart start =
      let queue = PriorityQueue<(int * int) * int * ((int * int) list), int>()
      queue.Enqueue((start, 0, []), 0)
      let visited = Map.empty

      let rec findShortest pathMap (queue: PriorityQueue<(int * int) * int * ((int * int) list), int>) =
          match queue.Count with
          | 0
          | _ when pathMap |> Map.containsKey target -> Some pathMap[target]
          | 0 when pathMap |> Map.containsKey target |> not -> None
          | _ ->
              let point, parentCost, path = queue.Dequeue()

              if pathMap |> Map.containsKey point then
                  findShortest pathMap queue
              else
                  let adjacentWithCost =
                      getAdjacent size point
                      |> List.filter (fun p -> pathMap |> Map.containsKey p |> not)
                      |> List.map (fun p -> (p, getCost p))

                  let newPath = (point :: path)
                  let newPathMap = pathMap |> Map.add point newPath

                  adjacentWithCost
                  |> List.iter (fun (point, cost) -> queue.Enqueue((point, parentCost + cost, newPath), parentCost + cost))

                  findShortest newPathMap queue

      findShortest visited queue
    startPoints
    |> List.map findForStart
    |> List.filter Option.isSome
    |> List.map (Option.get >> (fun path -> (List.length path) - 1))

let shortestTake2 (grid: char [] []) =
    let size = (Array.length grid[0], Array.length grid)
    let sizeX, sizeY = size

    let allPairs =
        Seq.allPairs (seq { 0 .. (sizeX - 1) }) (seq { 0 .. (sizeY - 1) })
        |> Seq.toList

    let start =
        allPairs
        |> List.find (fun (x, y) -> grid[y][x] = 'S')

    let target =
        allPairs
        |> List.find (fun (x, y) -> grid[y][x] = 'E')

    let queue = PriorityQueue<(int * int), int>()
    queue.Enqueue(start, 0)

    let getValue (x, y) = grid[y][x]

    let canStep (x, y) (dx, dy) =
        let mutable value = getValue (x, y)
        let comparison = getValue (dx, dy)
        if value = 'S' then
          value <- 'a'

        if comparison = 'E' then
            value = 'z' || value = 'y'
        elif comparison >= value then
            (int comparison - int value) <= 1
        else
            true

    let getAdjacent size (x, y) =
        deltas
        |> List.map (fun (dx, dy) -> (x + dx, y + dy))
        |> List.filter (inBounds size)
        |> List.filter (canStep (x, y))

    let addCostsToMap costMap points path =
        let shouldAddCostToMap costMap (point, cost) =
            if costMap |> Map.containsKey point
               && costMap[point] |> fst <= cost then
                false
            else
                true

        let addCostToMap pathCost (point, cost) = pathCost |> Map.add point (cost, path)

        points
        |> List.filter (shouldAddCostToMap costMap)
        |> List.fold addCostToMap costMap

    let rec step (costMap: Map<int * int, int * ((int * int) list)>) visited (queue: PriorityQueue<(int * int), int>) =
        match queue.Count with
        | 0
        | _ when visited |> Set.contains target -> costMap[target] |> snd
        | _ ->
            let point = queue.Dequeue()

            if visited |> Set.contains point then
                step costMap visited queue
            else
                let cost, path = costMap[point]

                let adjacentWithCost =
                    getAdjacent size point
                    |> List.filter (fun p -> visited |> Set.contains p |> not)
                    |> List.map (fun p -> (p, cost + 1))

                let newPathCost = addCostsToMap costMap adjacentWithCost (point :: path)
                let newVisisted = visited |> Set.add point

                adjacentWithCost
                |> List.iter (fun (point, cost) -> queue.Enqueue(point, cost))

                step newPathCost newVisisted queue


    let pathCost = [ start, (0, []) ] |> Map.ofList
    step pathCost Set.empty queue

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> Array.toList |> getGrid


let data = loadData ()

data
|> findShortestPathCost (fun ch -> ch = 'S')
|> List.head
|> printfn "Part 1: %d"

data
|> findShortestPathCost (fun ch -> ch = 'S' || ch = 'a')
|> List.sort
|> List.head
|> printfn "Part 2: %d"
