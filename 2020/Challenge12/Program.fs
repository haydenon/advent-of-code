open System

type Direction =
  | North
  | South
  | East
  | West

type Action =
  | Direction of Direction * int
  | Rotate of int
  | Forward of int

let parseDirection op arg =
  let dir =
    match op with
    | 'N' -> North
    | 'S' -> South
    | 'E' -> East
    | 'W' -> West
    | _   -> failwith "Invalid input"
  Direction(dir, arg)

let parseRow (text : string) =
  let op = text.[0]
  let arg = text.Substring(1) |> Int32.Parse
  match op with
  | 'N' | 'S' | 'E' | 'W' -> parseDirection op arg
  | 'L' -> Rotate (-arg / 90)
  | 'R' -> Rotate (arg / 90)
  | 'F' -> Forward arg
  | _   -> failwith "Invalid input"

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseRow
  |> Array.toList

let moveInDir dir dist (x,y) =
  let (xd, yd) =
    match dir with
    | North -> (0, -dist)
    | South -> (0, dist)
    | West -> (-dist, 0)
    | East -> (dist, 0)
  (x + xd, y + yd)

let rotate dir steps =
  let getStep = function | North -> 0 | East  -> 1 | South -> 2 | West  -> 3
  let getDir = function | 0 -> North | 1 -> East | 2 -> South | _ -> West
  let step = getStep dir
  getDir (((step + steps) + 4) % 4)

let rec followDirections actions dir coords =
  match actions with
  | action :: tail ->
    match action with
    | Direction (moveDir, dist) ->
      followDirections tail dir (moveInDir moveDir dist coords)
    | Rotate steps ->
      followDirections tail (rotate dir steps) coords
    | Forward dist ->
      followDirections tail dir (moveInDir dir dist coords)
  | [] -> coords

let rotateWaypoint (x, y) steps =
  let steps = (steps + 4) % 4 // Get positive
  match steps with
  | 1 -> (y * -1 , x)
  | 2 -> (x * -1, y * -1)
  | 3 -> (y, x * -1)
  | _ -> failwith "Invalid rotation"

let moveToWaypoint (sx, sy) (wx, wy) dist =
  let (xd, yd) = (wx * dist, wy * dist)
  (sx + xd, sy + yd)

let rec followWaypoint actions ship waypoint =
  match actions with
  | action :: tail ->
    match action with
    | Direction (moveDir, dist) ->
      followWaypoint tail ship (moveInDir moveDir dist waypoint)
    | Rotate steps ->
      followWaypoint tail ship (rotateWaypoint waypoint steps)
    | Forward dist ->
      followWaypoint tail (moveToWaypoint ship waypoint dist) waypoint
  | [] -> ship

[<EntryPoint>]
let main _ =
  let data = loadData()
  let (x, y) = followDirections data East (0,0)
  printfn "Ended at (%d,%d), Manhattan distance of %d with instruction set one" x y (Math.Abs(x) + Math.Abs(y))

  let (x, y) = followWaypoint data (0,0) (10, -1)
  printfn "Ended at (%d,%d), Manhattan distance of %d with instruction set two" x y (Math.Abs(x) + Math.Abs(y))
  0