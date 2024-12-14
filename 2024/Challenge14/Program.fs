open System
open System.Text.RegularExpressions

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let parseLine (line: string) =
        let parseNumbers (nums: string) =
            let parts = nums.Split("=")
            let numbers = parts[ 1 ].Split(",") |> Array.map int32
            (numbers[0], numbers[1])

        let parts = line.Split(" ")
        (parseNumbers parts[0], parseNumbers parts[1])

    text |> Array.map parseLine |> Array.toList

let data = loadData ()

let rec runRounds (width, height) rounds robots =
    if rounds = 0 then
        robots
    else
        let getWrapped value clamp =
            if value < 0 then
                clamp + value
            else
                value % clamp

        let runRobot ((x, y), (dx, dy)) =
            (getWrapped (x + dx) width, getWrapped (y + dy) height), (dx, dy)

        let nextRobots = robots |> List.map runRobot
        runRounds (width, height) (rounds - 1) (nextRobots)

let width, height = 101, 103

let quadrants =
    [ (Seq.allPairs (seq { 0 .. (width / 2) - 1 }) (seq { 0 .. (height / 2) - 1 }))
      |> Seq.toList
      |> Set.ofList
      (Seq.allPairs (seq { 0 .. (width / 2) - 1 }) (seq { (height / 2) + 1 .. (height - 1) }))
      |> Seq.toList
      |> Set.ofList
      (Seq.allPairs (seq { (width / 2) + 1 .. width - 1 }) (seq { 0 .. (height / 2) - 1 }))
      |> Seq.toList
      |> Set.ofList
      (Seq.allPairs (seq { (width / 2) + 1 .. width - 1 }) (seq { (height / 2) + 1 .. (height - 1) }))
      |> Seq.toList
      |> Set.ofList ]

let countInQuadrants robots =
    let coords = robots |> List.map fst

    quadrants
    |> List.fold
        (fun acc quad ->
            acc
            * (coords
               |> List.filter (fun coord -> quad |> Set.contains coord)
               |> List.length))
        1

data
|> runRounds (width, height) 100
|> countInQuadrants
|> printfn "%d"
