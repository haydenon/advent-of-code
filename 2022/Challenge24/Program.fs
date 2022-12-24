open System
open System.Collections.Generic

type Dir =
    | North
    | South
    | East
    | West

let parseBlizzards (lines: string list) =


    let height = (lines |> List.length) - 2
    let width = (lines[0].Length) - 2

    let north = Array.init height (fun _ -> Array.init width (fun _ -> false))
    let south = Array.init height (fun _ -> Array.init width (fun _ -> false))
    let east = Array.init height (fun _ -> Array.init width (fun _ -> false))
    let west = Array.init height (fun _ -> Array.init width (fun _ -> false))

    let getArray ch =
        match ch with
        | '^' -> north
        | 'v' -> south
        | '>' -> east
        | '<' -> west
        | _ -> failwith "Invalid input"

    let fillRow (row, line: string) =
        let line = line.Substring(1, line.Length - 2)

        line.ToCharArray()
        |> Array.indexed
        |> Array.filter (snd >> ((=) '.') >> not)
        |> Array.iter (fun (col, ch) -> (getArray ch).[row].[col] <- true)

    lines
    |> List.skip 1
    |> List.take height
    |> List.indexed
    |> List.iter fillRow

    (width, height), (north, south, east, west)

type Blizzards = (bool [] [] * bool [] [] * bool [] [] * bool [] [])

let hasBlizzardAtStep (width, height) ((north, south, east, west): Blizzards) step (x, y) =
    if y < 0 || y >= height then
        false
    else
        [ ((x + step) % width, y), west
          (((x - (step % width) + width) % width), y), east
          (x, (y + step) % height), north
          (x, (y - (step % height) + height) % height), south ]

        |> List.exists (fun ((x, y), blizzards) -> blizzards.[y].[x])

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> Array.toList |> parseBlizzards

type Actions =
    | Wait
    | Move of Dir

type ActionNode = ((int * int) * int * (int * int) list)

let deltas = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]

let getAdjacent (width, height) (x, y) =
    deltas
    |> List.map (fun (dx, dy) -> (x + dx), (y + dy))
    |> List.filter (fun (x, y) ->
        (x = 0 && y = -1)
        || (x = width - 1 && y = height)
        || (x >= 0 && y >= 0 && x < width && y < height))

let findShortestPath startStep (blizzards: Blizzards) (startX, startY) (targetX, targetY) =
    let blizzardArr = blizzards |> (fun (first, _, _, _) -> first)
    let size = blizzardArr[0] |> Array.length, blizzardArr |> Array.length
    let (width, height) = size
    let duration = width * height
    let startNode = ((startX, startY), startStep, [])

    let queue = PriorityQueue<ActionNode, int>()
    queue.Enqueue(startNode, 0)

    let getOptions (x, y) steps =
        let options = (x, y) :: getAdjacent size (x, y)

        options
        |> List.filter (
            (hasBlizzardAtStep size blizzards (steps + 1))
            >> not
        )

    let addCosts path options steps =
        options
        |> List.map (fun (x, y) ->
            ((x, y), steps + 1, (x, y) :: path), (steps + 1 + abs (targetX - x) + abs (targetY - y)))

    let rec step visited (queue: PriorityQueue<ActionNode, int>) =
        match queue.Count with
        | 0 -> failwith "Could not find path"
        | _ ->
            let point = queue.Dequeue()
            let ((x, y), steps, path) = point

            if x = targetX && y = targetY then
                path
            elif visited |> Set.contains (x, y, steps % duration) then
                step visited queue
            else
                let visited = visited |> Set.add (x, y, steps % duration)
                let options = getOptions (x, y) steps
                // printfn "%d,%d %d %A" x y steps options

                let optionsWithCost = addCosts path options steps

                optionsWithCost
                |> List.iter (fun (node, cost) -> queue.Enqueue(node, cost))

                step visited queue


    step Set.empty queue

let size, data = loadData ()

let tx, ty = size
let target = (tx - 1, ty - 1)

let part1 =
    findShortestPath 0 data (0, -1) target
    |> List.length
    |> ((+) 1)


part1 |> printfn "Part 1: %d"

let goingBack =
    findShortestPath part1 data (tx - 1, ty) (0, 0)
    |> List.length
    |> ((+) 1)

let andBackAgain =
    findShortestPath (part1 + goingBack) data (0, -1) (tx - 1, ty - 1)
    |> List.length
    |> ((+) 1)

printfn "Part 2: %d" (andBackAgain + goingBack + part1)