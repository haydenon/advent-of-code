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


let rec followInstructions (grid: GridValue [] []) instructions dir (x, y) =
    let moveInDir (x, y) dir =
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

    let moveOneInDir (x, y) dir =
        let (newX, newY) = moveInDir (x, y) dir
        let mutable nx = newX
        let mutable ny = newY

        while grid[ny][nx] <> Open && grid[ny][nx] <> Wall do
            let (newX, newY) = moveInDir (nx, ny) dir
            nx <- newX
            ny <- newY

        if grid[ny][nx] = Wall then
            (x, y)
        else
            (nx, ny)

    let moveInDir (x, y) amount dir =
        seq { 0 .. (amount - 1) }
        |> Seq.fold (fun coords _ -> moveOneInDir coords dir) (x, y)

    match instructions with
    | [] -> (dir, (x, y))
    | instruction :: rest ->
        match instruction with
        | Right
        | Left -> followInstructions grid rest (rotate dir instruction) (x, y)
        | Move num ->
            let newCoords = moveInDir (x, y) num dir
            followInstructions grid rest dir newCoords

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

followInstructions grid instructions East (firstCoords, 0)
|> scoreForDirLoc
|> printfn "Part 1: %d"
