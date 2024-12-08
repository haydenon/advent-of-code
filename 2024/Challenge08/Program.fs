open System
open System.Linq

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    let width = text[0].Length
    let height = text.Length
    let rows = seq { 0 .. height - 1 }
    let cols = seq { 0 .. width - 1 }
    let allCoords = Seq.allPairs rows cols
    let grid = text |> Array.map (fun str -> str |> Seq.toArray)

    let antennae =
        allCoords
        |> Seq.fold
            (fun acc (x, y) ->
                match grid[y][x] with
                | '.' -> acc
                | ch ->
                    acc
                    |> Map.change ch (function
                        | None -> Some [ (x, y) ]
                        | Some list -> Some((x, y) :: list)))
            Map.empty

    (width, height), antennae

let (dims, antennae) = loadData ()

//   1 2 3 4
// 1 x

// 2   a b

// 3   b a

// 4       x

let findAntennae dims antennae =
    let width,height = dims
    let inBounds x y =
      x >= 0 && x < width && y >= 0 && y < height
    let getNodes (_, coords) =
        let getAntinode (coord1, coord2) =
            if coord1 = coord2 then
                None
            else
                let (x1, y1), (x2, y2) = coord1, coord2
                let anx = x1 - (x2 - x1)
                let any = y1 - (y2 - y1)
                if inBounds anx any then
                  Some(anx, any)
                else
                  None

        coords
        |> List.allPairs coords
        |> List.choose getAntinode

    antennae
    |> Map.toList
    |> List.collect getNodes
    |> Set.ofList

findAntennae dims antennae |> Set.count |> printfn "Part 1: %d"
