open System

type Segment =
  | LeftTop
  | LeftBottom
  | Top
  | Center
  | Bottom
  | RightTop
  | RightBottom

let allLocations =
  [
    LeftTop
    LeftBottom
    Top
    Center
    Bottom
    RightTop
    RightBottom
  ]

let digitPatterns = [
  (0, [LeftTop; LeftBottom; Top; Bottom; RightTop; RightBottom]);
  (1, [RightTop; RightBottom]);
  (2, [LeftBottom; Top; Center; Bottom; RightTop]);
  (3, [Top; Center; Bottom; RightTop; RightBottom]);
  (4, [LeftTop; Center; RightTop; RightBottom]);
  (5, [LeftTop; Top; Center; Bottom; RightBottom]);
  (6, [LeftTop; LeftBottom; Top; Center; Bottom; RightBottom]);
  (7, [Top; RightTop; RightBottom]);
  (8, [LeftTop; LeftBottom; Top; Center; Bottom; RightTop; RightBottom])
  (9, [LeftTop; Top; Center; Bottom; RightTop; RightBottom]);
]

let patternSets =
  digitPatterns
  |> List.map (fun (_, pattern) -> Set.ofList pattern)

let uniqueCount =
  digitPatterns
  |> List.filter (fun (num, _) -> List.contains num [1;4;7;8])

let parseLine (line : string) =
  let parseValues (values : string) =
    values.Split(" ")
    |> Array.map (fun chars -> chars.ToCharArray() |> Set.ofArray)
    |> Array.toList
  let values = line.Split(" | ")
  (parseValues values[0], parseValues values[1])

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseLine
  |> Array.toList

let rec addDeductions possibilities locations pattern chars =
  let removeOtherChars charSet =
    charSet
    |> Set.filter(fun c -> chars |> Set.contains c)
  let removeChars charSet =
    charSet
    |> Set.filter(fun c -> chars |> Set.contains c |> not)
  match locations with
  | [] -> possibilities
  | loc :: tail when List.contains loc pattern ->
    let newPoss =
      possibilities
      |> Map.change loc (Option.map removeOtherChars)
    addDeductions newPoss tail pattern chars
  | loc :: tail ->
    let newPoss =
      possibilities
      |> Map.change loc (Option.map removeChars)
    addDeductions newPoss tail pattern chars

let rec bruteForce isValid segments remaining mapping =
  let trySegmentAsChar segment tail char =
    let newChars = remaining |> List.filter ((<>) char)
    let newMap =
      mapping
      |> Map.add char segment
    bruteForce isValid tail newChars newMap
  match segments with
  | [] ->
    match isValid mapping with
    | true -> Some mapping
    | false -> None
  | segment :: tail ->
    remaining
    |> List.tryPick (trySegmentAsChar segment tail)

let isValid (wires : Set<char> list) mapping =
  let onlyOnePossibility segments =
    patternSets
    |> List.filter (fun set -> set = segments)
    |> List.length
    |> ((=) 1)
  wires
  |> List.map (Set.map (fun c -> Map.find c mapping))
  |> List.forall onlyOnePossibility

let getNumberFromWire (mapping : Map<char, Segment>) (wires : Set<char> list) =
  let getSegmentsForWire wire =
    wire |> Set.map (fun c -> Map.find c mapping)
  let getNumberForSegments segments =
    let (number, _) =
      digitPatterns
      |> List.find (fun (_, set) -> set |> Set.ofList = segments)
    number
  wires
  |> List.map (getSegmentsForWire >> getNumberForSegments)

let rec decodePatterns remaining wires mapping =
  match wires with
  | [] ->
    mapping
  | wire :: tail ->
    let length = Set.count wire
    let lengthMatch =
      remaining
      |> List.filter (fun (_, pattern) -> List.length pattern = length)
      |> List.tryExactlyOne
    match lengthMatch with
    | Some numberPattern ->
      let (number, _) = numberPattern
      let remaining = remaining |> List.filter (fun np -> np <> numberPattern)
      mapping
      |> Map.add wire number
      |> decodePatterns remaining tail
    | None ->
      decodePatterns remaining tail mapping

let isMatch mappings output =
  mappings
  |> Map.exists (fun chars _ -> chars = output)

let createCharSet() = seq{0..6} |> Seq.map (fun i -> i + int 'a' |> char) |> Set.ofSeq

let createPossibilities () =
  seq { 1..(List.length allLocations) }
  |> Seq.map (fun _ -> createCharSet())
  |> List.ofSeq
  |> List.zip allLocations
  |> Map.ofList

let data = loadData()
let allWires = data |> List.map (fun (input, output) -> List.append input output)
let mappings = allWires |> List.map (fun wires -> decodePatterns uniqueCount wires Map.empty)
let allOutputs = data |> List.map (fun (_, output) ->  output)
let matchCount =
  List.zip mappings allOutputs
  |> List.map (fun (mappings, outputs) -> outputs |> List.filter (isMatch mappings) |> List.length)
  |> List.sum

printfn "Part 1: %d" matchCount

let chars = createCharSet() |> Set.toList
let segmentMappings =
  allWires
  |> List.map ((fun wires -> bruteForce (isValid wires) allLocations chars Map.empty) >> Option.get)
let outputs =
  List.zip allOutputs segmentMappings
  |> List.map (fun (wires, mapping) -> getNumberFromWire mapping wires)
  |> List.map ((fun values -> String.Join("", values)) >> Int32.Parse)
let result = outputs |> List.sum

printfn "Part 2: %A" result
