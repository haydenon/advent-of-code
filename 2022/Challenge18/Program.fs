open System
open System.Collections.Generic

let parsePos (line: string) =
    let parts = line.Split(",")
    let parse = Int32.Parse
    (parts[0] |> parse, parts[1] |> parse, parts[2] |> parse)

let allPairs3 seq1 seq2 seq3 =
    (Seq.allPairs seq2 seq3)
    |> Seq.allPairs seq1
    |> Seq.map (fun (x, (y, z)) -> (x, y, z))


let makeGrid positions =
    let xPos = positions |> List.map (fun (x, _, _) -> x)
    let yPos = positions |> List.map (fun (_, y, _) -> y)
    let zPos = positions |> List.map (fun (_, _, z) -> z)
    let minx, maxx = xPos |> List.min, xPos |> List.max
    let miny, maxy = yPos |> List.min, yPos |> List.max
    let minz, maxz = zPos |> List.min, zPos |> List.max

    let xSize = (maxx - minx) + 1
    let ySize = (maxy - miny) + 1
    let zSize = (maxz - minz) + 1

    let grid =
        Array.init zSize (fun _ -> Array.init ySize (fun _ -> Array.init xSize (fun _ -> false)))

    let hasCoord (x, y, z) =
        positions
        |> List.exists (fun (px, py, pz) -> px = x && py = y && pz = z)

    allPairs3 (seq { minx..maxx }) (seq { miny..maxy }) (seq { minz..maxz })
    |> Seq.iter (fun (x, y, z) -> grid.[z - minz].[y - miny].[x - minx] <- hasCoord (x, y, z))

    grid

let getSizes (grid: bool [] [] []) =
    let xSize = grid[0][0] |> Array.length
    let ySize = grid[0] |> Array.length
    let zSize = grid |> Array.length
    (xSize, ySize, zSize)

let outOfBounds grid (x, y, z) =
    let xSize, ySize, zSize = getSizes grid

    x < 0
    || y < 0
    || z < 0
    || x >= xSize
    || y >= ySize
    || z >= zSize

let lavaAt grid (x, y, z) =
    if outOfBounds grid (x, y, z) then
        false
    else
        grid[z].[y].[x]

let deltas =
    [ (-1, 0, 0)
      (1, 0, 0)
      (0, -1, 0)
      (0, 1, 0)
      (0, 0, -1)
      (0, 0, 1) ]


let rec cantFindBoundary found grid visited toVisit =
    match toVisit with
    | [] -> found, visited
    | (x, y, z) :: rest ->
        let visited = visited |> Set.add (x, y, z)

        let touchingEdge =
            deltas
            |> List.exists (fun (dx, dy, dz) -> outOfBounds grid (x + dx, y + dy, z + dz))

        let air =
            deltas
            |> List.map (fun (dx, dy, dz) -> (x + dx, y + dy, z + dz))
            |> List.filter (fun dcoords ->
                not(outOfBounds grid dcoords)
                && not (lavaAt grid dcoords)
                && not (visited |> Set.contains dcoords))

        let found = if touchingEdge then false else found
        let toVisit = List.append air rest
        cantFindBoundary found grid visited toVisit

let isInternalAir (visitedAir: Dictionary<int * int * int, bool>) grid (x, y, z) =
    if visitedAir.ContainsKey(x, y, z) then
        visitedAir[(x, y, z)]
    elif lavaAt grid (x, y, z) then
        false
    else
        let cantFindBoundaryRes, newVisited =
            cantFindBoundary true grid Set.empty [ (x, y, z) ]

        for visited in newVisited do
            visitedAir.Add(visited, cantFindBoundaryRes)

        cantFindBoundaryRes

let surfaceAreaAt grid internalAir (x, y, z) =
    deltas
    |> List.map (fun (dx, dy, dz) -> (x + dx, y + dy, z + dz))
    |> List.filter (fun coords -> lavaAt grid coords || internalAir |> Set.contains coords)
    |> List.length
    |> fun l -> 6 - l

let getCoolingRate excludeInternal grid =
    let xSize, ySize, zSize = getSizes grid

    let allCoords =
        allPairs3 (seq { 0 .. (xSize - 1) }) (seq { 0 .. (ySize - 1) }) (seq { 0 .. (zSize - 1) })

    let withLava = allCoords |> Seq.filter (lavaAt grid)

    let internalAir =
        if excludeInternal then
            Set.empty
        else
            let visited = Dictionary<int * int * int, bool>()

            let internalAir =
                allCoords
                |> Seq.fold
                    (fun internalVals coords ->

                        match isInternalAir visited grid coords with
                        | true -> coords :: internalVals
                        | false -> internalVals)
                    []
            internalAir |> Set.ofList


    let surfaceArea =
        withLava
        |> Seq.map (surfaceAreaAt grid internalAir)
        |> Seq.sum

    surfaceArea


let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text
    |> Array.toList
    |> List.map parsePos
    |> makeGrid

let data = loadData ()

data
|> getCoolingRate true
|> printfn "Part 1: %d"

data
|> getCoolingRate false
|> printfn "Part 2: %d"
