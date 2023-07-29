open System

type Dir =
    | Up
    | Down
    | Left
    | Right

let parseWire (wire: string) =
    let parseSection (section: string) =
        let dir =
            match section[0] with
            | 'U' -> Up
            | 'D' -> Down
            | 'L' -> Left
            | 'R' -> Right
            | _ -> failwith "Invalid direction"

        (dir, section.Substring(1) |> Int32.Parse)

    wire.Split(",")
    |> Array.map parseSection
    |> Array.toList

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    (text[0] |> parseWire, text[1] |> parseWire)

let sectionCoords (x, y) (dir, count) =
    seq { 1..count }
    |> Seq.map (fun delta ->
        match dir with
        | Up -> (x, y - delta)
        | Down -> (x, y + delta)
        | Left -> (x - delta, y)
        | Right -> (x + delta, y))
    |> List.ofSeq

let rec getPoints points loc wire =
    match wire with
    | section :: rest ->
        let coords = sectionCoords loc section

        let newPoints =
            coords
            |> List.fold (fun set coord -> coord :: set) points

        let newLoc = coords |> List.last
        getPoints newPoints newLoc rest
    | [] -> points

let getWireCoords (a, b) =
    (getPoints [] (0, 0) a |> List.rev, getPoints [] (0, 0) b |> List.rev)

let getIntersections (a, b) =
    (a |> Set.ofList, b |> Set.ofList)
    |> fun (a, b) -> Set.intersect a b
    |> Set.toList

let data = loadData ()

let (coordsA, coordsB) = data |> getWireCoords

let intersections = (coordsA, coordsB) |> getIntersections

intersections
|> List.map (fun (x, y) -> abs x + abs y)
|> List.sort
|> List.find (fun dist -> dist <> 0)
|> printfn "Part 1: %d"

intersections
|> List.map (fun coords ->
    let indA = coordsA |> List.findIndex ((=) coords)
    let indB = coordsB |> List.findIndex ((=) coords)
    indA + indB + 2)
|> List.min
|> printfn "Part 2: %d"
