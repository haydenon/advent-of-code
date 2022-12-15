open System

type Sensor = Sensor of (int * int) * int * (int * int)

let getDistance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

let parseSensor (line: string) =
    let parseCoords (coords: string) =
        let parts = coords.Split(", ")
        let parseAxis (axis: string) = axis.Split("=")[1] |> Int32.Parse
        (parts[0] |> parseAxis, parts[1] |> parseAxis)

    let parts = line.Split(": closest beacon is at ")
    let sensor, beacon = parts[0], parts[1]

    let (sx, sy) =
        sensor.Substring("Sensor at ".Length)
        |> parseCoords

    let (bx, by) = beacon |> parseCoords
    let distance = getDistance (sx, sy) (bx, by)

    Sensor((sx, sy), distance, (bx, by))

let findAtRow row sensors =
    let (minx, maxx) =
        ((sensors
          |> List.map (function
              | Sensor ((x, _), dist, _) -> x - dist)
          |> List.min),
         (sensors
          |> List.map (function
              | Sensor ((x, _), dist, _) -> x + dist))
         |> List.max)

    let isCovered (x, y) =
        sensors
        |> List.exists (function
            | Sensor (sensor, distance, _) -> getDistance sensor (x, y) <= distance)

    let hasBeacon (x, y) =
        sensors
        |> List.exists (function
            | Sensor (_, _, (bx, by)) -> bx = x && by = y)

    Seq.zip (seq { minx..maxx }) (Seq.replicate (maxx - minx + 1) row)
    |> Seq.map (fun coord -> isCovered coord && not (hasBeacon coord))
    |> Seq.toList

let findPosition sensors =
    let getRangeAtRow row (Sensor ((x, y), dist, _)) =
        let offset = abs (row - y)
        let widthAtRow = abs (dist - offset)
        (x - widthAtRow, x + widthAtRow)

    let getRangesForRow row =
        let inRange =
            sensors
            |> List.filter (function
                | Sensor ((_, y), dist, _) -> y + dist >= row && y - dist <= row)

        inRange
        |> List.map (getRangeAtRow row)
        |> List.sortBy fst

    let rec mergeRanges newRanges ranges =
        match ranges with
        | (s1, e1) :: (s2, e2) :: rest when e1 >= s2 ->
            mergeRanges newRanges ((s1, (if e1 > e2 then e1 else e2)) :: rest)
        | (s1, e1) :: (s2, e2) :: rest -> mergeRanges ((s1, e1) :: newRanges) ((s2, e2) :: rest)
        | s1 :: rest -> mergeRanges (s1 :: newRanges) rest
        | [] -> newRanges |> List.sortBy fst

    let (y, ranges) =
      seq { 0 .. Int32.MaxValue }
      |> Seq.map (getRangesForRow >> (mergeRanges []))
      |> Seq.indexed
      |> Seq.find (fun (_, ranges) -> (List.length ranges) > 1)
    ((ranges[0] |> snd) + 1, y)

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.toList |> List.map parseSensor

let data = loadData ()

findAtRow 2000000 data
|> List.filter id
|> List.length
|> printfn "Part 1: %d"

let (x,y) = findPosition data
(int64 x) * 4000000L + (int64 y)
|> printfn "Part 2: %d"
