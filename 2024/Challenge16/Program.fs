open System
open System.Collections.Generic

type Direction =
    | North
    | South
    | East
    | West

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    let parseMap (map: string array) = map |> Array.map Seq.toArray
    parseMap text

let data = loadData ()

let allCoords =
    Seq.allPairs (seq { 0 .. data[0].Length - 1 }) (seq { 0 .. data.Length - 1 })

let start =
    allCoords
    |> Seq.find (fun (x, y) -> data[y][x] = 'S')

let endPoint =
    allCoords
    |> Seq.find (fun (x, y) -> data[y][x] = 'E')

let getNext (x, y) =
    function
    | North -> (x, y - 1)
    | South -> (x, y + 1)
    | East -> (x + 1, y)
    | West -> (x - 1, y)

let getTurns =
    function
    | North -> [ East; West ]
    | South -> [ East; West ]
    | East -> [ North; South ]
    | West -> [ North; South ]

type VisitedMap = Map<(int32 * int32) * Direction, (int32 * (((int32 * int32) * Direction) list list))>

let getAdjacentAndCost (board: char array array) (visited: VisitedMap) (coords, dir) =
    ((getNext coords dir, dir), 1)
    :: (getTurns dir
        |> List.map (fun d -> ((coords, d), 1000)))
    |> List.filter (fun (((x, y), dir), _) ->
        not (visited |> Map.containsKey ((x, y), dir))
        && board[y][x] <> '#')

let rec findBestPath
    exitEarly
    (getAdjacent: VisitedMap -> ((int32 * int32) * Direction) -> (((int32 * int32) * Direction) * int32) list)
    (queue: PriorityQueue<((int32 * int32) * Direction) * (int32 * (((int32 * int32) * Direction) list)), int32>)
    (visited: VisitedMap)
    =
    if queue.Count = 0 then
        let pathsForDirs =
            visited
            |> Map.toList
            |> List.filter (fun ((coord, _), _) -> coord = endPoint)

        let minCost =
            pathsForDirs
            |> List.minBy (fun (_, (cost, _)) -> cost)
            |> snd
            |> fst

        let allPaths =
            pathsForDirs
            |> List.filter (fun (_, (c, _)) -> c = minCost)
            |> List.collect (fun (_, (_, paths)) -> paths)

        (minCost, allPaths)
    else
        let (next, (costToHere, pathToHere)) = queue.Dequeue()

        if next |> fst = endPoint && exitEarly then
            (costToHere, [ pathToHere |> List.rev ])
        else
            let newVisited =
                visited
                |> Map.change (next) (function
                    | Some (currCost, currPaths) ->
                        if currCost < costToHere then
                            Some(currCost, currPaths)
                        else if currCost = costToHere then
                            Some(currCost, pathToHere :: currPaths)
                        else
                            Some(costToHere, [ pathToHere ])
                    | None -> Some(costToHere, [ pathToHere ]))

            let adjacent = getAdjacent newVisited next

            for (coordAndDir, cost) in adjacent do
                let newCost = costToHere + cost
                queue.Enqueue((coordAndDir, (newCost, coordAndDir :: pathToHere)), newCost)

            findBestPath exitEarly getAdjacent queue newVisited

let getAdjacent = getAdjacentAndCost data
let startingQueue = PriorityQueue()
startingQueue.Enqueue(((start, East), (0, [])), 0)

findBestPath true getAdjacent startingQueue Map.empty
|> fst
|> printfn "Part 1: %d"

let startingQueue2 = PriorityQueue()
startingQueue2.Enqueue(((start, East), (0, [])), 0)

// There's a bug here... it's off by one - I need to come back and fix this
findBestPath false getAdjacent startingQueue2 Map.empty
|> snd
|> List.collect (List.map fst)
|> List.distinct
|> List.length
|> printfn "Part 2: %d"
