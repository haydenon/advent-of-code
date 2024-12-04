open System
open System.Text.RegularExpressions

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.map Seq.toArray

let data = loadData ()

let countInGrid increments chars (grid: char [] []) =
    let gridX = grid[0] |> Array.length
    let gridY = grid |> Array.length

    let inBounds x y =
        x >= 0 && x < gridX && y >= 0 && y < gridY

    let rec checkPos increment chars index (x, y) =
        match chars with
        | char :: rest ->
            // printfn "%d %d = %c %c %b %b" x y char (grid[y][x]) (inBounds x y) (grid[y][x] = char)
            if (inBounds x y) && grid[y][x] = char then
                checkPos increment rest (index + 1) (increment index (x, y))
            else
                false
        | _ -> true

    let checkAllCoords coords increment =
        coords |> Seq.filter (checkPos increment chars 0)

    let rows = seq { 0 .. gridY - 1 }
    let cols = seq { 0 .. gridX - 1 }
    let allCoords = Seq.allPairs rows cols

    increments
    |> List.map (checkAllCoords allCoords)
    |> List.map (Seq.length)
    |> List.sum

let part1Chars = [ 'X'; 'M'; 'A'; 'S' ]

let part1Increments =
    [ (fun _ (x, y) -> (x + 1, y))
      (fun _ (x, y) -> (x, y + 1))
      (fun _ (x, y) -> (x - 1, y))
      (fun _ (x, y) -> (x, y - 1))
      (fun _ (x, y) -> (x + 1, y + 1))
      (fun _ (x, y) -> (x - 1, y + 1))
      (fun _ (x, y) -> (x + 1, y - 1))
      (fun _ (x, y) -> (x - 1, y - 1)) ]

data
|> countInGrid part1Increments part1Chars
|> printfn "Part 1: %d"

let part2Chars = [ 'M'; 'M'; 'A'; 'S'; 'S' ]

let createSeqFun (sequence: (int * int) seq) =
    let sequence = Seq.toArray sequence

    fun idx (x, y) ->
        if idx >= Array.length sequence then
            0, 0
        else
            let (dx, dy) = sequence[idx]
            x + dx, y + dy

let part2Increments =
    [ createSeqFun [ (0, 2)
                     (1, -1)
                     (1, -1)
                     (0, 2) ]
      createSeqFun [ (2, 0)
                     (-1, -1)
                     (-1, -1)
                     (2, 0) ]
      createSeqFun [ (0, -2)
                     (-1, 1)
                     (-1, -1)
                     (0, 2) ]
      createSeqFun [ (-2, 0)
                     (1, 1)
                     (-1, 1)
                     (2, 0) ] ]

data
|> countInGrid part2Increments part2Chars
|> printfn "Part 2: %d"
