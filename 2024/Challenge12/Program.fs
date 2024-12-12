open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.map (fun str -> str |> Seq.toArray)

let data = loadData ()

let adjacent = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

let getAdjacent (x, y) =
    adjacent
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))

let count (grid: char array array) =
    let width, height = grid[0].Length, grid.Length

    let matches (x, y) (ox, oy) =
        if ox < 0 || ox >= width || oy < 0 || oy >= height then
            false
        else
            grid[y][x] = grid[oy][ox]

    let rec countBlock (visited: Set<int * int>, perimeter: int, area: int) coords =
        if (visited |> Set.contains coords) then
            (visited, perimeter, area)
        else
            let adjacent = getAdjacent coords

            let newPerimeter =
                (adjacent
                 |> List.filter (matches coords >> not)
                 |> List.length)
                + perimeter

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
            let input = (vis, 0, 0)
            let res = countBlock input coord

            match res with
            | (vis, _, 0) -> (vis, areas)
            | (vis, per, area) -> (vis, (per, area) :: areas))
        (Set.empty, [])
    |> snd



data
|> count
|> List.map (fun (per, area) -> per * area)
|> List.sum
|> printfn "Part 1: %d"
