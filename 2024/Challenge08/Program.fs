let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    let width = text[0].Length
    let height = text.Length
    let rows = seq { 0 .. height - 1 }
    let cols = seq { 0 .. width - 1 }
    let allCoords = Seq.allPairs rows cols
    let grid = text |> Array.map (fun str -> str |> Seq.toArray)

    let antennae =
        allCoords
        |> Seq.fold
            (fun acc (x, y) ->
                match grid[y][x] with
                | '.' -> acc
                | ch ->
                    acc
                    |> Map.change ch (function
                        | None -> Some [ (x, y) ]
                        | Some list -> Some((x, y) :: list)))
            Map.empty

    (width, height), antennae

let (dims, antennae) = loadData ()

let findAntennae part1Rules dims antennae =
    let width, height = dims

    let inBounds (x, y) =
        x >= 0 && x < width && y >= 0 && y < height

    let getNodes (_, coords) =
        let getAntinode (coord1, coord2) =
            if coord1 = coord2 then
                []
            else
                let (x1, y1), (x2, y2) = coord1, coord2
                let dx = (x2 - x1)
                let dy = (y2 - y1)

                if part1Rules then
                    let anx = x1 - dx
                    let any = y1 - dy

                    if inBounds (anx, any) then
                        [ (anx, any) ]
                    else
                        []
                else
                    let rec getCoords dx dy (x, y) list =
                        let next = x - dx, y - dy

                        if not (inBounds next) then
                            list
                        else
                            getCoords dx dy next (next :: list)

                    getCoords dx dy (x2, y2) []

        coords
        |> List.allPairs coords
        |> List.collect getAntinode

    antennae
    |> Map.toList
    |> List.collect getNodes
    |> Set.ofList

findAntennae true dims antennae
|> Set.count
|> printfn "Part 1: %d"

findAntennae false dims antennae
|> Set.count
|> printfn "Part 2: %d"
