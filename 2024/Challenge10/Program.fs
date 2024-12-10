open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text
    |> Array.map (Seq.toArray >> (Array.map (string >> int32)))

let data = loadData ()

let adjacent = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

let getAdjacent x y =
    adjacent
    |> Seq.map (fun (dx, dy) -> x + dx, y + dy)

let inBounds (width, height) (x, y) =
    x >= 0 && x < width && y >= 0 && y < height

let getDims (grid: int array array) = grid[0].Length, grid.Length

let scoreTrailhead addEnd (grid: int array array) (x, y) =
    let inBounds = inBounds (getDims grid)

    let rec score (x, y) visited count =
        let num = grid[y][x]

        if num = 9 then
            count + 1,
            if addEnd then
                (visited |> Set.add (x, y))
            else
                visited
        else
            let toVisit =
                getAdjacent x y
                |> Seq.filter (fun (x, y) ->
                    inBounds (x, y)
                    && grid[y][x] = (num + 1)
                    && not (visited |> Set.contains (x, y)))

            toVisit
            |> Seq.fold (fun (count, visited) coord -> score coord visited count) (count, visited)

    score (x, y) Set.empty 0 |> fst

let findStarts (grid: int array array) =
    let (width, height) = getDims grid

    Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })
    |> Seq.filter (fun (x, y) -> grid[y][x] = 0)


data
|> findStarts
|> Seq.sumBy (scoreTrailhead true data)
|> printfn "Part 1: %d"

data
|> findStarts
|> Seq.sumBy (scoreTrailhead false data)
|> printfn "Part 2: %d"