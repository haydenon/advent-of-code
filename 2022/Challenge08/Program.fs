open System

let makeGrid count initial =
    let initFun _ = initial
    Array.init count (fun _ -> Array.init count initFun)

let parseGrid (lines: string list) =
    let count = List.length lines
    let grid = makeGrid count 0

    let rec initLine row (lines: string list) =
        match lines with
        | line :: rest ->
            seq { 0 .. (count - 1) }
            |> Seq.iter (fun col -> grid[row][col] <- Int32.Parse(string line.[col]))

            initLine (row + 1) rest
        | [] -> grid

    initLine 0 lines

let findVisibleTrees (grid: int [] []) =
    let count = (Array.length grid)
    let visibleGrid = makeGrid count false

    let checkFromDirection (getIndex, coords) =
        let currentMaxes = Array.init count (fun _ -> -1)

        let rec checkAllCoords coords =
            match coords with
            | coord :: rest ->

                let (x, y) = coord

                if currentMaxes[getIndex coord] < grid[y][x] then
                    visibleGrid[y][x] <- true
                    currentMaxes[getIndex coord] <- grid[y][x]

                checkAllCoords rest
            | [] -> ()

        checkAllCoords (Seq.toList coords)

    let directions =
        [ (fun (x, _) -> x), (Seq.allPairs (seq { 0 .. (count - 1) }) (seq { 0 .. (count - 1) }))
          (fun (_, y) -> y), (Seq.allPairs (seq { 0 .. (count - 1) }) (seq { 0 .. (count - 1) }))
          (fun (x, _) -> x),
          (Seq.allPairs (seq { 0 .. (count - 1) }) (seq { 0 .. (count - 1) })
           |> Seq.rev)
          (fun (_, y) -> y),
          (Seq.allPairs (seq { 0 .. (count - 1) }) (seq { 0 .. (count - 1) }))
          |> Seq.rev ]

    directions |> List.iter checkFromDirection

    visibleGrid


let findMaxTreeViewScore (grid: int [] []) =
    let count = (Array.length grid)

    let getTreeViewScore (x, y) =
        let rec scoreInDirection trees coords =
            let invalid dx dy =
                (dx = x && dy = y)
                || dx < 0
                || dx >= count
                || dy < 0
                || dy >= count

            match coords with
            | (dx, dy) :: rest ->
                if invalid dx dy then
                    trees
                elif grid[dy][dx] >= grid[y][x] then
                    trees + 1
                else
                    let newCurrent = grid[dy][dx]
                    scoreInDirection (trees + 1) rest
            | [] -> trees

        [ Seq.zip (seq { 0 .. (x - 1) } |> Seq.rev) (Seq.replicate count y)
          Seq.zip (Seq.replicate count x) (seq { 0 .. (y - 1) } |> Seq.rev)
          Seq.zip (seq { (x + 1) .. (count - 1) }) (Seq.replicate count y)
          Seq.zip (Seq.replicate count x) (seq { (y + 1) .. (count - 1) }) ]
        |> List.map (Seq.toList >> (scoreInDirection 0))
        |> List.reduce (*)

    Seq.allPairs (seq { 1 .. (count - 2) }) (seq { 1 .. (count - 2) })
    |> Seq.map getTreeViewScore
    |> Seq.toList


let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> List.ofArray |> parseGrid


let data = loadData ()

data
|> findVisibleTrees
|> Array.collect (fun v -> v |> Array.filter id)
|> Array.length
|> printfn "Part 1: %d"


data
|> findMaxTreeViewScore
|> List.max
|> printfn "Part 2: %d"
