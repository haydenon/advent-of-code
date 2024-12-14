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

let adjacent =
    [ (1, 0)
      (1, 1)
      (1, -1)
      (0, 1)
      (0, -1)
      (-1, 1)
      (-1, 0)
      (-1, -1) ]

let inBounds (x, y) =
    x >= 0 && x < width && y >= 0 && y < height

let countMaxAdjacent robots =
    let robots = robots |> List.map fst
    let coords = robots |> Set.ofList

    let rec count num (x, y) visited =
        if
            visited |> Set.contains (x, y)
            || not (coords |> Set.contains (x, y))
        then
            None
        else
            let adj =
                adjacent
                |> List.map (fun (dx, dy) -> x + dx, y + dy)
                |> List.filter (fun coord ->
                    coords |> Set.contains coord
                    && not (visited |> Set.contains coord))

            let newVisited = visited |> Set.add (x,y)
            Some(
                adj
                |> List.fold
                    (fun (acc, visit) next ->
                        match count acc next visit with
                        | Some acc -> acc
                        | None -> (acc, visit))
                    (num + 1, newVisited)
            )

    // Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })
    robots
    |> Seq.fold
        (fun (counts, visited) coord ->
            match count 0 coord visited with
            | Some (num, visit) -> num :: counts, visit
            | None -> (counts, visited))
        ([], Set.empty)
    |> fst
    |> List.max

let rec runUntilAdjacent count rounds robots =
    let next = runRounds (width, height) 1 robots
    let maxAdjacent = countMaxAdjacent next

    if maxAdjacent > count / 3 then
        (rounds + 1), next
    else
        runUntilAdjacent count (rounds + 1) next

let printRobots (num, robots) =
    let coords = robots |> List.map fst |> Set.ofList

    let getRow row =
        seq { 0 .. width - 1 }
        |> Seq.map (fun col ->
            if coords |> Set.contains (col, row) then
                '#'
            else
                '.')

    seq { 0 .. height - 1 }
    |> Seq.map (getRow >> Seq.toArray >> (fun s -> String s))
    |> Seq.iter (printfn "%s")
    printfn "Part 2: %d rounds" num

data
|> runRounds (width, height) 100
|> countInQuadrants
|> printfn "Part 1: %d"

data
|> runUntilAdjacent (data |> List.length) 0
|> printRobots
