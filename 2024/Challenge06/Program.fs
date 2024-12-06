open System
open System.Linq

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    let grid = text |> Array.map (fun str -> str |> Seq.toArray)
    let width = text[0].Length
    let height = text.Length
    let rows = seq { 0 .. height - 1 }
    let cols = seq { 0 .. width - 1 }
    let allCoords = Seq.allPairs cols rows

    let start =
        allCoords
        |> Seq.find (fun (x, y) -> grid[y][x] = '^')

    let blocks =
        allCoords
        |> Seq.filter (fun (x, y) -> grid[y][x] = '#')
        |> Set.ofSeq

    ((width, height), start, blocks)


type Orientation =
    | North
    | East
    | South
    | West

let nextOrientation =
    function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let nextCoords orientation (x, y) =
    match orientation with
    | North -> x, y - 1
    | East -> x + 1, y
    | South -> x, y + 1
    | West -> x - 1, y

let outOfBounds (width, height) x y =
    x < 0 || x >= width || y < 0 || y >= height

let rec findLocations dims blocks locations coords orientation =
    let (nx, ny) = nextCoords orientation coords

    if outOfBounds dims nx ny then
        locations
    else if blocks |> Set.contains (nx, ny) then
        findLocations dims blocks locations coords (nextOrientation orientation)
    else
        findLocations dims blocks (locations |> Set.add (nx, ny)) (nx, ny) orientation


let (dims, start, blocks) = loadData ()

findLocations dims blocks (Set.empty |> Set.add start) start North
|> Set.count
|> printfn "%A"
