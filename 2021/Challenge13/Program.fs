open System

type Instruction =
  | X of int
  | Y of int

let printGrid (grid : bool[][]) =
  for row in grid do
    let chars =
      row
      |> Array.map (fun v -> if v then "#" else ".")
    printfn "%s" (String.Join("", chars))

let rec parseCoords coords (lines : string list) =
  match lines with
  | "" :: tail ->
    let (maxX, _) = coords |> List.maxBy fst
    let (_, maxY) = coords |> List.maxBy snd
    let grid = Array.init (maxY + 1) (fun _ -> Array.create (maxX + 1) false)
    coords |> List.iter (fun (x,y) -> grid[y][x] <-true)
    (grid, lines |> List.filter (String.IsNullOrWhiteSpace >> not))
  | line :: tail ->
    let coordValues = line.Split(",")
    let point = (coordValues[0] |> Int32.Parse, coordValues[1] |> Int32.Parse)
    parseCoords (point :: coords) tail
  | [] -> failwith ""

let parseInstruction dir number =
  match dir with
  | "x" -> X(number |> Int32.Parse)
  | "y" -> Y(number |> Int32.Parse)
  | _ -> failwith "Invalid instruction"

let rec parseInstructions instructions  (lines : string list) =
  match lines with
  | [] -> instructions |> List.rev
  | text :: tail ->
    let parts = text.Substring(String.length "fold along ").Split("=")
    let instruction = parseInstruction parts[0] parts[1]
    parseInstructions (instruction :: instructions) tail

let loadData () =
  let coords, lines =
    System.IO.File.ReadAllLines("./input.txt")
    |> Array.toList
    |> parseCoords []
  let instructions = parseInstructions [] lines
  (instructions, coords)

let getTransformCoords (grid : bool[][]) instruction =
  match instruction with
  | X _ -> fun (x, y) -> [((Array.length grid[0]) - 1 - x,y); (x,y)]
  | Y _ -> fun (x, y) -> [(x, (Array.length grid) - 1 - y); (x,y)]

let getNewGrid (grid : bool[][]) instruction =
  match instruction with
  | X x -> Array.init (Array.length grid) (fun _ -> Array.create x false)
  | Y y -> Array.init y (fun _ -> Array.create (Array.length grid[0]) false)

let rec fold (grid : bool[][]) instructions =
  match instructions with
  | [] -> grid
  | instruction :: tail ->
    let newGrid = getNewGrid grid instruction
    let getTransformedCoords = getTransformCoords grid instruction
    let fillInPoint point =
      let transformCoords = getTransformedCoords point
      let (x,y) = point
      newGrid[y][x] <- transformCoords |> List.exists (fun (x,y) -> grid[y][x])
    let allCoords = Seq.allPairs (seq { 0..((Array.length newGrid[0]) - 1)}) (seq { 0..(Array.length newGrid - 1)})
    allCoords |> Seq.iter fillInPoint
    fold newGrid tail

let (instructions, grid) = loadData()
fold grid (instructions |> List.take 1)
|> Array.collect id
|> Array.filter id
|> Array.length
|> printfn "Part 1: %d"

printfn "Part 2:"
fold grid instructions
|> printGrid