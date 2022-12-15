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

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.toList |> List.map parseSensor

let data = loadData ()

findAtRow 2000000 data
|> List.filter id
|> List.length
|> printfn "%A"
