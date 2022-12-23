open System

let rec parseLocations coords row (lines: string list) =
    match lines with
    | [] -> coords
    | line :: rest ->
        let newCoords =
            line.ToCharArray()
            |> Array.indexed
            |> Array.filter (fun (_, ch) -> ch = '#')
            |> Array.fold (fun coords (col, _) -> coords |> Map.add (col, row) ()) coords

        parseLocations newCoords (row + 1) rest

type Direction =
    | North
    | South
    | East
    | West

let nextDir =
    function
    | North -> South
    | South -> West
    | West -> East
    | East -> North

let getDirs =
    function
    | North -> [ North; South; West; East ]
    | South -> [ South; West; East; North ]
    | West -> [ West; East; North; South ]
    | East -> [ East; North; South; West ]

let getMoveInDir coords (x, y) dir =
    let toCheck =
        match dir with
        | North ->
            [ (x, y - 1)
              (x - 1, y - 1)
              (x + 1, y - 1) ]
        | South ->
            [ (x, y + 1)
              (x - 1, y + 1)
              (x + 1, y + 1) ]
        | East ->
            [ (x + 1, y)
              (x + 1, y + 1)
              (x + 1, y - 1) ]
        | West ->
            [ (x - 1, y)
              (x - 1, y + 1)
              (x - 1, y - 1) ]

    let isEmpty (x, y) = coords |> Map.containsKey (x, y) |> not

    if toCheck |> List.forall isEmpty then
        Some(
            (x, y),
            (match dir with
             | North -> (x, y - 1)
             | South -> (x, y + 1)
             | East -> (x + 1, y)
             | West -> (x - 1, y))
        )
    else
        None

let getMove coords dir (x, y) =
    getDirs dir
    |> List.tryPick (getMoveInDir coords (x, y))

let moveCoords coords oldCoords newCoords =
    coords
    |> Map.remove oldCoords
    |> Map.add newCoords ()

let deltas =
    [ (-1, -1)
      (0, -1)
      (1, -1)
      (1, 0)
      (1, 1)
      (0, 1)
      (-1, 1)
      (-1, 0) ]

let hasAdjacent coords (x, y) =
    deltas
    |> List.exists (fun (dx, dy) -> coords |> Map.containsKey (x + dx, y + dy))

let rec runRounds endIf count coords dir =
    if endIf count coords then
        count, coords
    else
        let actualMoves =
            coords
            |> Map.keys
            |> Seq.filter (hasAdjacent coords)
            |> Seq.map (getMove coords dir)
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
            |> Seq.groupBy snd
            |> Seq.filter (fun (_, allAtCoords) -> Seq.length allAtCoords < 2)

        let newCoords =
            actualMoves
            |> Seq.fold
                (fun coords (_, moveSeq) ->
                    let (oldCoords, newCoords) = moveSeq |> Seq.head
                    moveCoords coords oldCoords newCoords)
                coords

        runRounds endIf (count + 1) newCoords (nextDir dir)

let boundingRect (coords: Map<int * int, unit>) =
    let coords = coords |> Map.toList |> List.map fst
    let xCoords = List.map fst coords
    let yCoords = List.map snd coords
    let minX, maxX = List.min xCoords, List.max xCoords
    let minY, maxY = List.min yCoords, List.max yCoords
    (minX, minY), (maxX, maxY)


let printGrid coords =
    let (minX, minY), (maxX, maxY) = boundingRect coords

    let printLine y =
        seq { minX..maxX }
        |> Seq.map (fun x ->
            if coords |> Map.containsKey ((x, y)) then
                '#'
            else
                '.')
        |> Seq.toArray
        |> fun chars -> new String(chars)
        |> printfn "%s"

    (seq { minY..maxY }) |> Seq.iter printLine

let getEmpty coords =
    let (minX, minY), (maxX, maxY) = boundingRect coords
    let count = coords |> Map.count
    let size = abs (maxX - minX + 1) * abs (maxY - minY + 1)
    size - count

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> Array.toList |> parseLocations Map.empty 0


let data = loadData ()

runRounds (fun count _ -> count = 10) 0 data North
|> snd
|> getEmpty
|> printfn "Part 1: %d"

let mutable prev = Map.empty

runRounds
    (fun _ coords ->
        if prev = coords then
            true
        else
            (prev <- coords
             false))
    0
    data
    North
|> fst
|> printfn "Part 2: %d"
