open System
open System.Collections.Generic

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let parseLine (line: string) =
        let parts = line.Split(",")
        parts[0] |> int, parts[1] |> int

    text |> Array.map parseLine |> Array.toList

let data = loadData ()

let width = 71
let height = 71

let allCoords = Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })

let start = (0, 0)

let endPoint = (width - 1, height - 1)

let getNext (x, y) =
    [ (x, y - 1)
      (x, y + 1)
      (x + 1, y)
      (x - 1, y) ]

let outOfBounds (x, y) =
    x < 0 || x >= width || y < 0 || y >= height

type VisitedMap = Map<(int32 * int32), int32>

let getAdjacentAndCost (board: bool array array) (visited: VisitedMap) (coords) =
    getNext coords
    |> List.filter (fun (x, y) ->
        not (outOfBounds (x, y))
        && not (visited |> Map.containsKey ((x, y)))
        && not (board[y][x]))
    |> List.map (fun coords -> (coords, 1))

let rec findBestPath
    exitEarly
    (getAdjacent: VisitedMap -> ((int32 * int32)) -> (((int32 * int32)) * int32) list)
    (queue: PriorityQueue<((int32 * int32)) * (int32 * (((int32 * int32)) list)), int32>)
    (visited: VisitedMap)
    =
    if queue.Count = 0 then
        let pathsForDirs =
            visited
            |> Map.toList
            |> List.filter (fun ((coord), _) -> coord = endPoint)

        let minCost =
            pathsForDirs
            |> List.minBy (fun (_, (cost)) -> cost)
            |> snd


        (minCost)
    else
        let (next, (costToHere, pathToHere)) = queue.Dequeue()

        if visited |> Map.containsKey next then
            findBestPath exitEarly getAdjacent queue visited
        else if next = endPoint && exitEarly then
            (costToHere)
        else
            let newVisited =
                visited
                |> Map.change (next) (function
                    | Some (currCost) ->
                        if currCost < costToHere then
                            Some(currCost)
                        // else if currCost = costToHere then
                        //     Some(currCost, [])
                        else
                            Some(costToHere)
                    | None -> Some(costToHere))

            let adjacent = getAdjacent newVisited next

            for (coordAndDir, cost) in adjacent do
                let newCost = costToHere + cost
                queue.Enqueue((coordAndDir, (newCost, coordAndDir :: pathToHere)), newCost)

            findBestPath exitEarly getAdjacent queue newVisited

let initBoard () =
    Array.init height (fun _ -> Array.init width (fun _ -> false))

let rec buildBoard (board: bool array array) count coords =
    match count, coords with
    | None, []
    | Some 0, _
    | Some _, [] -> board
    | Some c, (x, y) :: rest ->
        board[y][x] <- true
        buildBoard board (Some(c - 1)) rest
    | None, (x, y) :: rest ->
        board[y][x] <- true
        buildBoard board None rest

let board = buildBoard (initBoard ()) (Some 1024) data

let getAdjacent = getAdjacentAndCost board
let startingQueue = PriorityQueue()
startingQueue.Enqueue((start, (0, [])), 0)

findBestPath true getAdjacent startingQueue Map.empty
|> printfn "Part 1: %d"

// let startingQueue2 = PriorityQueue()
// startingQueue2.Enqueue(((start, East), (0, [])), 0)

// findBestPath false getAdjacent startingQueue2 Map.empty
// |> snd
// |> List.collect (List.map fst)
// |> List.append [start]
// |> List.distinct
// |> List.length
// |> printfn "Part 2: %d"
