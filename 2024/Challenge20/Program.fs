open System
open System.Collections.Generic

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.map Array.ofSeq

let grid = loadData ()

let width = grid[0].Length
let height = grid.Length
let allCoords = Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })

let start =
    allCoords
    |> Seq.find (fun (x, y) -> grid[y][x] = 'S')

let endPoint =
    allCoords
    |> Seq.find (fun (x, y) -> grid[y][x] = 'E')

let adjacent = [ (-1, 0); (1, 0); (0, -1); (0, 1) ]

let getAdjacent x y =
    adjacent
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))
    |> List.filter (fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)


let rec getCosts visited (x, y) costs path cost =
    if grid[y][x] = 'E' then
        ((x, y), cost) :: costs, (x, y) :: path
    else
        let next =
            getAdjacent x y
            |> List.find (fun (nx, ny) ->
                not (visited |> Set.contains (nx, ny))
                && grid[ny][nx] <> '#')

        let newVisited = visited |> Set.add (x, y)
        let newCosts = ((x, y), cost) :: costs
        getCosts newVisited next newCosts ((x, y) :: path) (cost + 1)

let (costList, path) = getCosts Set.empty start [] [] 0
let max = costList |> List.head |> snd

let costs =
    costList
    |> List.map (fun (coord, cost) -> (coord, max - cost))
    |> Map.ofList

type Coord = int * int

let getCheatsFromPoint costs (x, y) : (Coord * Coord * int) list =
    let usualCost = costs |> Map.find (x, y)

    let getCheatResults (wx, wy) =
        getAdjacent wx wy
        |> List.filter (fun (cx, cy) ->
            (grid[cy][cx] = '.' || grid[cy][cx] = 'E')
            && costs |> Map.find (cx, cy) < (usualCost - 2))
        |> List.map (fun (cheatCoord) -> ((wx, wy), cheatCoord, (usualCost - 2) - (costs |> Map.find cheatCoord)))

    let walls =
        getAdjacent x y
        |> List.filter (fun (nx, ny) -> grid[ny][nx] = '#')

    walls |> List.collect getCheatResults

let rec getCheats costs path cheats =
    match path with
    | [] -> cheats
    | coord :: rest ->
        let cheapsFromPoint = getCheatsFromPoint costs coord
        getCheats costs rest (List.append cheapsFromPoint cheats)

getCheats costs path []
|> List.filter (fun (_,_,savings) -> savings  >= 100)
|> List.length
|> printfn "%d"
// |> List.fold
//     (fun acc (c1, c2, savings) ->
//         acc
//         |> Map.change savings (function
//             | Some count -> Some(count + 1)
//             | None -> Some 1))
//     Map.empty
// |> printfn "%A"
