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
let part1Start = 1024
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
        match visited |> Map.tryFind endPoint with
        | Some count -> Some count
        | None -> None
    else
        let (next, (costToHere, pathToHere)) = queue.Dequeue()

        if visited |> Map.containsKey next then
            findBestPath exitEarly getAdjacent queue visited
        else if next = endPoint && exitEarly then

            match visited |> Map.tryFind endPoint with
            | Some count -> Some (if costToHere < count then costToHere else count)
            | None -> Some costToHere
        else
            let newVisited =
                visited
                |> Map.change (next) (function
                    | Some (currCost) ->
                        if currCost < costToHere then
                            Some(currCost)
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

let board = buildBoard (initBoard ()) (Some part1Start) data


let getAdjacent = getAdjacentAndCost board
let startingQueue = PriorityQueue()
startingQueue.Enqueue((start, (0, [])), 0)

findBestPath true getAdjacent startingQueue Map.empty
|> Option.get
|> printfn "Part 1: %d"


let index =
  seq { part1Start .. (width * height) }
  |> Seq.choose (fun i ->
      let getAdjacent = getAdjacentAndCost (buildBoard (initBoard ()) (Some i) data)
      let queue = PriorityQueue()
      queue.Enqueue((start, (0, [])), 0)
      match findBestPath true getAdjacent queue Map.empty with
      | Some _ -> Some i
      | None -> None
  )
  |> Seq.max


printfn "Part2: %d,%d" (data[index] |> fst) (data[index] |> snd)
// let startingQueue2 = PriorityQueue()
// startingQueue2.Enqueue(((start, East), (0, [])), 0)

// findBestPath false getAdjacent startingQueue2 Map.empty
// |> snd
// |> List.collect (List.map fst)
// |> List.append [start]
// |> List.distinct
// |> List.length
// |> printfn "Part 2: %d"
