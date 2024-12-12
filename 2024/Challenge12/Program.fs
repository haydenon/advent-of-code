open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.map (fun str -> str |> Seq.toArray)

let data = loadData ()

let adjacent = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

let getAdjacent (x, y) =
    adjacent
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))


let diagonal = [ (1, 1); (1, -1); (-1, 1); (-1, -1) ]

let getDiagonal (x, y) =
    diagonal
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))


let count (grid: char array array) =
    let width, height = grid[0].Length, grid.Length

    let matches (x, y) (ox, oy) =
        if ox < 0 || ox >= width || oy < 0 || oy >= height then
            false
        else
            grid[y][x] = grid[oy][ox]

    let rec countBlock (visited: Set<int * int>, perimeter: ((int * int) * (int * int)) list, area: int) coords =
        if (visited |> Set.contains coords) then
            (visited, perimeter, area)
        else
            let adjacent = getAdjacent coords

            let newPerimeterPieces =
                (adjacent |> List.filter (matches coords >> not))
                |> List.map (fun c -> (c, coords))

            let newPerimeter = perimeter |> List.append newPerimeterPieces

            let newVisited = visited |> Set.add coords
            let newArea = area + 1

            adjacent
            |> List.filter (matches coords)
            |> List.filter (fun other -> visited |> Set.contains other |> not)
            |> List.fold (fun acc newCoords -> countBlock acc newCoords) (newVisited, newPerimeter, newArea)

    let allCoords = Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })

    allCoords
    |> Seq.fold
        (fun (vis, areas) coord ->
            let input = (vis, [], 0)
            let res = countBlock input coord

            match res with
            | (vis, _, 0) -> (vis, areas)
            | (vis, per, area) -> (vis, (per, area) :: areas))
        (Set.empty, [])
    |> snd


let countSides (perimeter: ((int * int) * (int * int)) list) =
    let unique = perimeter |> List.map fst |> Set.ofList

    let sideToAdjacent =
        perimeter
        |> List.fold
            (fun acc (edge, inner) ->
                acc
                |> Map.change edge (function
                    | None -> Some 1
                    | Some count -> Some(count + 1)))
            Map.empty

    let visited = Set.empty

    let rec count visited coords sides perimeter =
        let adj = getAdjacent coords |> Set.ofList
        let diagonal = getDiagonal coords |> Set.ofList
        let reachable = perimeter |> Set.intersect (diagonal + adj)

        if (reachable |> Set.isEmpty) then
            sides
        else
            let newVisited = (visited |> Set.add coords)

            let intersect = adj |> Set.intersect perimeter
            let adjacent = intersect - newVisited


            let onSameSide = adjacent |> Set.toList |> List.tryHead
            // adjacent |> Set.toListperimeter |> S
            // |> Set (fun other ->
            //     not (visited |> Set.contains other)
            //     && adjacent |> List.contains other)

            match onSameSide with
            | Some next -> count newVisited next sides (perimeter |> Set.remove coords)
            | None ->
                let corners = perimeter |> Set.intersect diagonal
                // |> Set.filter (fun other ->
                //     not (newVisited |> Set.contains other)
                //     && diagonal |> List.contains other)

                corners
                |> Set.map (fun next ->
                    count newVisited next (sides + (sideToAdjacent |> Map.find next)) (perimeter |> Set.remove coords))
                |> Set.toList
                |> List.max

    let first = perimeter |> List.head |> fst
    let res = count visited first 1 (unique |> Set.remove first)
    printfn "%d" res
    res

let result = data |> count

result
|> List.map (fun (per, area) -> (per |> List.length) * area)
|> List.sum
|> printfn "Part 1: %d"

result
|> List.map (fun (per, area) -> (per |> countSides) * area)
|> List.sum
|> printfn "Part 1: %d"
