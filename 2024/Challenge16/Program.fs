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

type VisitedMap = Map<(int32 * int32) * Direction, (int32 * (((int32 * int32) * Direction) list))>

let getAdjacentAndCost (board: char array array) (visited: VisitedMap) (coords, dir) =
    ((getNext coords dir, dir), 1)
    :: (getTurns dir
        |> List.map (fun d -> ((coords, d), 1000)))
    |> List.filter (fun (((x, y), dir), _) ->
        not (visited |> Map.containsKey ((x, y), dir))
        && board[y][x] <> '#')

let rec findBestPath
    (getAdjacent: VisitedMap -> ((int32 * int32) * Direction) -> (((int32 * int32) * Direction) * int32) list)
    (queue: PriorityQueue<((int32 * int32) * Direction) * (int32 * (((int32 * int32) * Direction) list)), int32>)
    (visited: VisitedMap)
    =
    if queue.Count = 0 then
        failwith "No route"
    else
        let (next, (costToHere, pathToHere)) = queue.Dequeue()

        if next |> fst = endPoint then
            (costToHere, pathToHere |> List.rev)
        else
            let newVisited =
                visited
                |> Map.change (next) (function
                    | Some (currCost, currPath) ->
                        if currCost < costToHere then
                            Some(currCost, currPath)
                        else
                            Some(costToHere, pathToHere)
                    | None -> Some(costToHere, pathToHere))

            let adjacent = getAdjacent newVisited next

            for (coordAndDir, cost) in adjacent do
                let newCost = costToHere + cost
                queue.Enqueue((coordAndDir, (newCost, coordAndDir :: pathToHere)), newCost)

            findBestPath getAdjacent queue newVisited

let getAdjacent = getAdjacentAndCost data
let startingQueue = PriorityQueue()

let add accStart (dir: Direction) =
    startingQueue.Enqueue(((start, dir), (0, [])), 0)
    accStart |> Map.add (start, dir) (0, [])

let a =
    [ East ]//North; East; South; East ]
    |> List.fold add Map.empty
// startingQueue.Enqueue((start, East), 0)
// startingQueue.Enqueue((start, South), 0)
// startingQueue.Enqueue((start, West), 0)

findBestPath getAdjacent startingQueue a
|> printfn "%A"
