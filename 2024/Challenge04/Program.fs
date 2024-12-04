open System
open System.Text.RegularExpressions

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.map Seq.toArray

let data = loadData ()

let countInGrid (grid: char [] []) =
    let gridX = grid[0] |> Array.length
    let gridY = grid |> Array.length

    let inBounds x y =
        x >= 0 && x < gridX && y >= 0 && y < gridY

    let rec checkPos increment chars (x, y) =
        match chars with
        | char :: rest ->
            // printfn "%d %d = %c %c %b %b" x y char (grid[y][x]) (inBounds x y) (grid[y][x] = char)
            if (inBounds x y) && grid[y][x] = char then
                checkPos increment rest (increment (x, y))
            else
                false
        | _ -> true

    let checkAllCoords coords increment =
        coords
        |> Seq.filter (checkPos increment [ 'X'; 'M'; 'A'; 'S' ])

    let rows = seq { 0 .. gridY - 1 }
    let cols = seq { 0 .. gridX - 1 }
    let allCoords = Seq.allPairs rows cols

    let increments =
        [ (fun (x, y) -> (x + 1, y))
          (fun (x, y) -> (x, y + 1))
          (fun (x, y) -> (x - 1, y))
          (fun (x, y) -> (x, y - 1))
          (fun (x, y) -> (x + 1, y + 1))
          (fun (x, y) -> (x - 1, y + 1))
          (fun (x, y) -> (x + 1, y - 1))
          (fun (x, y) -> (x - 1, y - 1)) ]

    increments
    |> List.map (checkAllCoords allCoords)
    |> List.map (Seq.length)
    |> List.sum

data |> countInGrid |> printfn "Part 1: %d"
