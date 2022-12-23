open System
open System.Text.RegularExpressions

type GridValue =
    | Open
    | Wall
    | NotPresent

type Instruction =
    | Left
    | Right
    | Move of int

type Direction =
    | North
    | South
    | East
    | West

let rec parseInstructions (text: string) =
    let parseInstruction instruction =
        match instruction with
        | "R" -> Right
        | "L" -> Left
        | _ -> Move(instruction |> Int32.Parse)

    Regex.Split(text, @"(R|L|\d+)")
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.map parseInstruction

let parseGrid (lines: string list) =
    let width =
        lines
        |> List.map (fun str -> str.Length)
        |> List.max

    let height = lines |> List.length

    let grid = Array.init height (fun _ -> Array.init width (fun _ -> NotPresent))

    lines
    |> List.iteri (fun row line ->
        line.ToCharArray()
        |> Array.iteri (fun col ch ->
            if ch = '.' then
                grid[row][col] <- Open
            elif ch = '#' then
                grid[row][col] <- Wall))

    grid

let rotate current dir =
    match current, dir with
    | North, Right -> East
    | North, Left -> West
    | South, Right -> West
    | South, Left -> East
    | East, Right -> South
    | East, Left -> North
    | West, Right -> North
    | West, Left -> South
    | _ -> current

let moveInDir (grid: GridValue [] []) (x, y) dir =
    match dir with
    | North ->
        (x,
         if y = 0 then
             (grid.Length - 1)
         else
             y - 1)
    | South -> (x, (y + 1) % grid.Length)
    | East -> ((x + 1) % grid[0].Length, y)
    | West ->
        ((if x = 0 then
              (grid[0].Length - 1)
          else
              x - 1),
         y)

let moveOneInDir (grid: GridValue [] []) (x, y) dir =
    let (newX, newY) = moveInDir grid (x, y) dir
    let mutable nx = newX
    let mutable ny = newY

    while grid[ny][nx] <> Open && grid[ny][nx] <> Wall do
        let (newX, newY) = moveInDir grid (nx, ny) dir
        nx <- newX
        ny <- newY

    if grid[ny][nx] = Wall then
        (x, y)
    else
        (nx, ny)

let moveInDirP1 (grid: GridValue [] []) (x, y) amount dir =
    (seq { 0 .. (amount - 1) }
     |> Seq.fold (fun coords _ -> moveOneInDir grid coords dir) (x, y),
     dir)

let size = 50

let startOf num = size * (num - 1)
let endOf num = size * num - 1

// -------------
// -- Example --
// -------------

// let cubeFaces =
//     [ [ (West, (startOf 3, startOf 1), (startOf 3, endOf 1))
//         (North, (startOf 2, startOf 2), (endOf 2, startOf 2)) ] // 1 -> 3
//       [ (North, (startOf 3, startOf 1), (endOf 3, startOf 1))
//         (North, (endOf 1, startOf 2), (startOf 1, startOf 2)) ] // 1 -> 2
//       [ (East, (endOf 3, startOf 1), (endOf 3, endOf 1))
//         (East, (endOf 4, endOf 3), (endOf 4, startOf 3)) ] // 1 -> 6
//       [ (South, (startOf 1, endOf 2), (endOf 1, endOf 2))
//         (South, (endOf 3, endOf 3), (startOf 3, endOf 3)) ] // 2 -> 5
//       [ (West, (startOf 1, startOf 2), (startOf 1, endOf 2))
//         (South, (endOf 4, endOf 3), (startOf 4, endOf 3)) ] // 2 -> 6
//       [ (South, (startOf 2, endOf 2), (endOf 2, endOf 2))
//         (West, (startOf 3, endOf 3), (startOf 3, startOf 3)) ] // 3 -> 5
//       [ (East, (endOf 3, startOf 2), (endOf 3, endOf 2))
//         (North, (endOf 4, startOf 3), (startOf 4, startOf 3)) ] ] // 4 -> 6

let cubeFaces =
    [ [ (North, (startOf 2, startOf 1), (endOf 2, startOf 1))
        (West, (startOf 1, startOf 4), (startOf 1, endOf 4)) ] // 1 -> 6
      [ (West, (startOf 2, startOf 1), (startOf 2, endOf 1))
        (West, (startOf 1, endOf 3), (startOf 1, startOf 3)) ] // 1 -> 4
      [ (South, (startOf 3, endOf 1), (endOf 3, endOf 1))
        (East, (endOf 2, startOf 2), (endOf 2, endOf 2)) ] // 2 -> 3
      [ (East, (endOf 3, startOf 1), (endOf 3, endOf 1))
        (East, (endOf 2, endOf 3), (endOf 2, startOf 3)) ] // 2 -> 5
      [ (North, (startOf 3, startOf 1), (endOf 3, startOf 1))
        (South, (startOf 1, endOf 4), (endOf 1, endOf 4)) ] // 2 -> 6
      [ (West, (startOf 2, startOf 2), (startOf 2, endOf 2))
        (North, (startOf 1, startOf 3), (endOf 1, startOf 3)) ] // 3 -> 4
      [ (South, (startOf 2, endOf 3), (endOf 2, endOf 3))
        (East, (endOf 1, startOf 4), (endOf 1, endOf 4)) ] ] // 5 -> 6

let oppositeOf =
    function
    | North -> South
    | South -> North
    | East -> West
    | West -> East


let getCoordSeq s e =
    if s = e then Seq.replicate size e
    elif s < e then seq { s..e }
    else seq { e..s } |> Seq.rev

let getOppositeCoordFacing idx (dir, (sx, sy), (ex, ey)) =
    let coords =
        Seq.zip (getCoordSeq sx ex) (getCoordSeq sy ey)
        |> List.ofSeq

    (oppositeOf dir, coords[idx])


let addEdgeToMap (dir, (sx, sy), (ex, ey)) opposite map =
    Seq.zip (getCoordSeq sx ex) (getCoordSeq sy ey)
    |> Seq.indexed
    |> Seq.fold
        (fun map (idx, (x, y)) ->
            map
            |> Map.add (dir, x, y) (getOppositeCoordFacing idx opposite))
        map

let nextMoveOnEdges =
    let map = Map.empty

    cubeFaces
    |> List.fold
        (fun map edgeList ->
            let e1 = edgeList[0]
            let e2 = edgeList[1]
            map |> addEdgeToMap e1 e2 |> addEdgeToMap e2 e1)
        map

let moveInDirP2 (x, y) dir =
    if nextMoveOnEdges |> Map.containsKey (dir, x, y) then
        nextMoveOnEdges[(dir, x, y)]
    else
        (dir,
         match dir with
         | North -> (x, y - 1)
         | South -> (x, y + 1)
         | East -> ((x + 1), y)
         | West -> (x - 1, y))

let moveOneInDirP2 (grid: GridValue [] []) (x, y) dir =
    let newDir, (newX, newY) = moveInDirP2 (x, y) dir

    if grid[newY][newX] = Wall then
        (x, y), dir
    else
        (newX, newY), newDir

let moveNumInDirP2 (grid: GridValue [] []) (x, y) amount dir =
    (seq { 0 .. (amount - 1) }
     |> Seq.fold (fun (coords, dir) _ -> moveOneInDirP2 grid coords dir) ((x, y), dir))


let rec followInstructions move (grid: GridValue [] []) instructions dir (x, y) =
    match instructions with
    | [] -> (dir, (x, y))
    | instruction :: rest ->
        match instruction with
        | Right
        | Left -> followInstructions move grid rest (rotate dir instruction) (x, y)
        | Move num ->
            let newCoords, dir = move grid (x, y) num dir
            followInstructions move grid rest dir newCoords

let numForDir =
    function
    | East -> 0
    | South -> 1
    | West -> 2
    | North -> 3

let scoreForDirLoc (dir, (x, y)) =
    1000 * (y + 1) + 4 * (x + 1) + (numForDir dir)

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let length = Array.length text
    let grid = parseGrid (text |> Array.take (length - 2) |> Array.toList)

    (grid,
     text[length - 1]
     |> parseInstructions
     |> Array.toList)

let (grid, instructions) = loadData ()

let firstCoords = grid[0] |> Array.findIndex (fun v -> v = Open)

followInstructions moveInDirP1 grid instructions East (firstCoords, 0)
|> scoreForDirLoc
|> printfn "Part 1: %d"

followInstructions moveNumInDirP2 grid instructions East (firstCoords, 0)
|> scoreForDirLoc
|> printfn "Part 2: %d"
