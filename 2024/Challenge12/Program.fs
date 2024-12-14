open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.map (fun str -> str |> Seq.toArray)

let data = loadData ()

let adjacent = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

let getAdjacent (x, y) =
    adjacent
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))


let diagonal = [ (1, 1); (1, -1); (-1, 1); (-1, -1) ]

let getDiagonal (x, y) =
    diagonal
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))


let count (grid: char array array) =
    let width, height = grid[0].Length, grid.Length

    let matches (x, y) (ox, oy) =
        if ox < 0 || ox >= width || oy < 0 || oy >= height then
            false
        else
            grid[y][x] = grid[oy][ox]

    let rec countBlock (visited: Set<int * int>, perimeter: ((int * int) * (int * int)) list, area: int) coords =
        if (visited |> Set.contains coords) then
            (visited, perimeter, area)
        else
            let adjacent = getAdjacent coords

            let newPerimeterPieces =
                (adjacent |> List.filter (matches coords >> not))
                |> List.map (fun c -> (c, coords))

            let newPerimeter = perimeter |> List.append newPerimeterPieces

            let newVisited = visited |> Set.add coords
            let newArea = area + 1

            adjacent
            |> List.filter (matches coords)
            |> List.filter (fun other -> visited |> Set.contains other |> not)
            |> List.fold (fun acc newCoords -> countBlock acc newCoords) (newVisited, newPerimeter, newArea)

    let allCoords = Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })

    allCoords
    |> Seq.fold
        (fun (vis, areas) coord ->
            let input = (vis, [], 0)
            let res = countBlock input coord

            match res with
            | (vis, _, 0) -> (vis, areas)
            | (vis, per, area) -> (vis, (per, area) :: areas))
        (Set.empty, [])
    |> snd

type Direction =
    | North
    | East
    | South
    | West

let countSides (perimeter: ((int * int) * (int * int)) list) =
    let runForPerimeter (perimeter: ((int * int) * (int * int)) list) =
        let start = perimeter |> List.head
        let insides = perimeter |> List.map snd |> Set.ofList
        let outsides = perimeter |> List.map fst |> Set.ofList

        let ((ox, oy), (ix, iy)) = start

        let dir =
            match ox - ix, oy - iy with
            | -1, _ -> North
            | 1, _ -> South
            | _, -1 -> East
            | _, 1 -> West
            | _ -> failwith "Invalid"

        let getNextInDirection (x, y) dir =
            match dir with
            | North -> (x, y - 1)
            | South -> (x, y + 1)
            | East -> (x + 1, y)
            | West -> (x - 1, y)

        let getInsideCornerDirChange dir =
            // We're navigating clockwise, so an inside corner is always anti-clockwise turn
            match dir with
            | North -> West
            | South -> East
            | East -> North
            | West -> South

        let getOutsideCornerDirChange (x, y) dir =
            // We're navigating clockwise, so an outside corner is always clockwise and around corner turn
            match dir with
            | North -> (x + 1, y - 1), East
            | South -> (x - 1, y + 1), West
            | East -> (x + 1, y + 1), South
            | West -> (x - 1, y - 1), North

        let isOutsideCorner coords dir =
            let cornerCoords = (getOutsideCornerDirChange coords dir |> fst)

            insides |> Set.contains cornerCoords |> not
            && outsides |> Set.contains cornerCoords

        let getChange coords dir =
            let nextInDir = getNextInDirection coords dir

            // Outside corner
            if isOutsideCorner coords dir then
                let nextCoords, nextDir = getOutsideCornerDirChange coords dir
                nextCoords, nextDir, 1
            else if outsides |> Set.contains nextInDir then
                nextInDir, dir, 0
            // Inside corner
            else if insides |> Set.contains nextInDir then
                coords, getInsideCornerDirChange dir, 1
            else
                failwith "Invalid case"

        let rec count start sides coords dir visited =
            let newVisited = visited |> Set.add coords

            if start = (dir, coords) && sides > 0 then
                sides, newVisited
            else
                let newCoords, newDir, sideDelta = getChange coords dir
                count start (sides + sideDelta) newCoords newDir newVisited

        let startOutside = start |> fst
        count (dir, startOutside) 0 startOutside dir Set.empty

    let rec run perimeter =
        let count, visited = runForPerimeter perimeter

        let remaining =
            perimeter
            |> List.filter (fun (out, _) -> visited |> Set.contains out |> not)

        if remaining |> List.isEmpty then
            count
        else
            count + run remaining

    run perimeter


let result = data |> count

result
|> List.map (fun (per, area) -> (per |> List.length) * area)
|> List.sum
|> printfn "Part 1: %d"

let mutable i = 0

result
|> List.map (fun (per, area) -> (per |> countSides) * area)
|> List.sum
|> printfn "Part 2: %d"
