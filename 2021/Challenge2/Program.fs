open System

type Direction =
  | Forward of int
  | Down of int
  | Up of int

let parseDir = function
  | "forward" -> Forward
  | "down" -> Down
  | "up" -> Up
  | _ -> failwith "Invalid direction"


let parseCommand (text : string) =
  let data = text.Split(" ")
  data[1]
  |> Int32.Parse
  |> parseDir data[0]

let loadData () =
  let text = System.IO.File.ReadAllLines("./input.txt")
  Array.map parseCommand text
  |> List.ofArray

let rec getPart1Coords directions (x,y) =
  match directions with
  | [] -> (x,y)
  | Forward delta :: rest -> getPart1Coords rest (x + delta, y)
  | Up delta :: rest -> getPart1Coords rest (x, y - delta)
  | Down delta :: rest -> getPart1Coords rest (x, y + delta)

let rec getPart2Coords directions aim (x,y) =
  match directions with
  | [] -> (x,y)
  | Forward delta :: rest -> getPart2Coords rest aim (x + delta, y + delta * aim)
  | Up delta :: rest -> getPart2Coords rest (aim - delta) (x, y)
  | Down delta :: rest -> getPart2Coords rest (aim + delta) (x, y)

let directions = loadData()

let (x1, y1) = getPart1Coords directions (0,0)
printfn "Part 1: %d" (x1 * y1)

let (x2, y2) = getPart2Coords directions 0 (0,0)
printfn "Part 2: %d" (x2 * y2)
