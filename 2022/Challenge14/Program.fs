open System

let rec parsePaths paths (lines: string list) =
    match lines with
    | [] -> paths
    | line :: rest ->
        let path =
            line.Split(" -> ")
            |> Array.map (fun point ->
                point.Split(",")
                |> Array.map Int32.Parse
                |> (fun point -> point[0], point[1]))
            |> Array.toList

        parsePaths (path :: paths) rest

let getOffset paths =
    let xPoints = paths |> List.map fst
    let yPoints = paths |> List.map snd
    ((List.min xPoints, List.min yPoints), (List.max xPoints, List.max yPoints))

let getGrid ((minx, miny), (maxx, maxy)) (paths: (int * int) list list) =
    let xSize = maxx - minx + 1
    let ySize = maxy + 1
    let grid = Array.init ySize (fun _ -> Array.init xSize (fun _ -> false))

    let fill (points: (int * int) seq) =

        points
        |> Seq.iter (fun (x, y) -> grid[y][x] <- true)

    paths
    |> List.map (List.pairwise)
    |> List.iter (
        List.iter (function
            | ((x1, y1), (x2, y2)) when x1 = x2 && y1 < y2 ->
                Seq.zip (Seq.replicate (y2 - y1 + 1) (x1 - minx)) (seq { y1..y2 })
                |> fill
            | ((x1, y1), (x2, y2)) when x1 = x2 && y1 > y2 ->
                Seq.zip (Seq.replicate (y1 - y2 + 1) (x1 - minx)) (seq { y2..y1 })
                |> fill
            | ((x1, y1), (x2, y2)) when y1 = y2 && x1 < x2 ->
                Seq.zip (seq { (x1 - minx) .. (x2 - minx) }) (Seq.replicate (x2 - x1 + 1) y1)
                |> fill
            | ((x1, y1), (x2, y2)) ->
                Seq.zip (seq { (x2 - minx) .. (x1 - minx) }) (Seq.replicate (x1 - x2 + 1) y1)
                |> fill)
    )

    grid


let printGrid (orig: bool [] []) (filled: bool [] []) =
    let xSize = Array.length orig[0]
    let ySize = Array.length orig

    let printLine line =
        Seq.zip (seq { 0 .. (xSize - 1) }) (Seq.replicate xSize line)
        |> Seq.map (fun (x, y) ->
            if orig[y][x] then '#'
            elif filled[y][x] then 'o'
            else '.')
        |> Seq.toArray
        |> (fun chars -> new System.String(chars))
        |> printfn "%s"


    (seq { 0 .. (ySize - 1) })
    |> Seq.toList
    |> List.iter printLine


let placeSand canGoOutOfBounds ((grid: bool [] []), ((minx, _), _)) =
    let xSize = Array.length grid[0]
    let ySize = Array.length grid
    let newGrid = Array.map (Array.map id) grid

    let outOfBounds (x, y) =
        x - minx < 0 || x - minx >= xSize || y >= ySize

    let isFilled (x, y) =
        if outOfBounds (x, y) then
            not (canGoOutOfBounds)
        else
            newGrid[y][x - minx]

    let fill (x, y) = newGrid[y][x - minx] <- true

    let rec placeSandUnit (sx, sy) =
        if outOfBounds (sx, sy) then
            if canGoOutOfBounds then
                None
            else
                Some(sx, sy)
        elif not (isFilled (sx, sy + 1)) then
            placeSandUnit (sx, sy + 1)
        elif not (isFilled (sx - 1, sy + 1)) then
            placeSandUnit (sx - 1, sy + 1)
        elif not (isFilled (sx + 1, sy + 1)) then
            placeSandUnit (sx + 1, sy + 1)
        else
            fill (sx, sy)
            Some(sx, sy)

    Seq.replicate 30000 (500, 0)
    |> Seq.map placeSandUnit
    |> Seq.takeWhile Option.isSome
    |> Seq.toList
    |> ignore

    newGrid


let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    let paths = text |> Array.toList |> parsePaths []
    let minMax = paths |> List.collect id |> getOffset
    getGrid minMax paths, minMax

let data = loadData ()

let grid = data |> fst

let originalCount =
    grid
    |> Array.collect id
    |> Array.filter id
    |> Array.length

let filled = data |> placeSand true

let newCount =
    filled
    |> Array.collect id
    |> Array.filter id
    |> Array.length

(newCount - originalCount) |> printfn "Part 1: %d"


let xSize = Array.length grid[0]
let ySize = Array.length grid
let minx = data |> snd |> fst |> fst

let part2Grid =
    Array.init (ySize + 2) (fun y ->
        Array.init (xSize + 800) (fun x ->
            if y < ySize && x >= 400 && (x - 400) < xSize then
                grid[y][x - 400]
            else
                false))

seq { 0 .. (xSize + 800 - 1) }
|> Seq.toList
|> List.iter (fun x -> part2Grid[ySize + 1][x] <- true)

let ((_, miny), (maxx, maxy)) = data |> snd
let minMax2 = ((minx - 400, miny), (maxx + 400, maxy + 2))
let part2Data = (part2Grid, minMax2)
let filled2 = part2Data |> placeSand false

let originalCount2 =
    part2Grid
    |> Array.collect id
    |> Array.filter id
    |> Array.length

let newCount2 =
    filled2
    |> Array.collect id
    |> Array.filter id
    |> Array.length

(newCount2 - originalCount2) |> printfn "Part 2: %d"