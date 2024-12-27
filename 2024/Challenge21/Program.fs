open System
open System.Collections.Generic

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.map List.ofSeq |> Array.toList

let numberCoords =
    Map.empty
    |> Map.add '7' (0, 0)
    |> Map.add '8' (1, 0)
    |> Map.add '9' (2, 0)
    |> Map.add '4' (0, 1)
    |> Map.add '5' (1, 1)
    |> Map.add '6' (2, 1)
    |> Map.add '1' (0, 2)
    |> Map.add '2' (1, 2)
    |> Map.add '3' (2, 2)
    |> Map.add '0' (1, 3)
    |> Map.add 'A' (2, 3)

let dirCoords =
    Map.empty
    |> Map.add '^' (1, 0)
    |> Map.add 'A' (2, 0)
    |> Map.add '<' (0, 1)
    |> Map.add 'v' (1, 1)
    |> Map.add '>' (2, 1)

let cachePaths coords (a, b) =
    let mapToDir =
        function
        | 1, 0 -> '>'
        | -1, 0 -> '<'
        | 0, 1 -> 'v'
        | 0, -1 -> '^'
        | _ -> failwith "Invalid"

    let rec getPathsInner (ax, ay) (bx, by) =
        if ax = bx && ay = by then
            [ [] ]
        else
            let xMove =
                if ax = bx then
                    []
                else
                    [ ((if ax > bx then -1 else 1), 0) ]

            let yMove =
                if ay = by then
                    []
                else
                    [ (0, (if ay > by then -1 else 1)) ]

            List.append xMove yMove
            |> List.filter (fun (dx, dy) ->
                coords
                |> Map.values
                |> Seq.contains (ax + dx, ay + dy))
            |> List.collect (fun (dx, dy) ->
                let inner = getPathsInner (ax + dx, ay + dy) (bx, by)

                inner
                |> List.map (fun path -> (mapToDir (dx, dy)) :: path))

    getPathsInner (coords |> Map.find a) (coords |> Map.find b)


let allNums = numberCoords |> Map.keys
let allNumCoords = numberCoords |> Map.values

let allNumPaths =
    Seq.allPairs allNums allNums
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.map (fun (a, b) -> ((a, b), cachePaths numberCoords (a, b)))
    |> Map.ofSeq

let allDirs = dirCoords |> Map.keys
let allDirCoords = dirCoords |> Map.values

let allDirPaths =
    Seq.allPairs allDirs allDirs
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.map (fun (a, b) -> ((a, b), cachePaths dirCoords (a, b)))
    |> Map.ofSeq

let dirs = [ '>'; '^'; '<'; 'v' ]

let pathCache = Dictionary<(char * char) * int, int64>()

let rec getBestPathCost source dest levels =
    let cacheKey = ((source, dest), levels)

    if pathCache.ContainsKey cacheKey then
        pathCache[cacheKey]
    else
        let pathMap =
            if (source = 'A' && dest = 'A') || dirs |> List.contains source
               || dirs |> List.contains dest then
                allDirPaths
            else
                allNumPaths

        if source = dest then
            1L
        else
            let pathsFromHere = pathMap |> Map.find (source, dest)

            let result =
                if levels = 0 then
                    pathsFromHere
                    |> List.map List.length
                    |> List.min
                    |> int64
                    |> ((+) 1L)
                else
                    pathsFromHere
                    |> List.map (fun path ->
                        'A' :: path @ ['A']
                        |> List.pairwise
                        |> List.sumBy (fun (src, dst) -> getBestPathCost src dst (levels - 1)))
                    |> List.min

            pathCache.Add(cacheKey, result)
            result

let rec evaluate levels coords currCoord keys output outputs =
    match keys with
    | [] ->
        let atLevel = output |> List.rev

        match levels with
        | [] -> (atLevel :: outputs) |> List.rev
        | (coords, start) :: rest -> evaluate rest coords start atLevel [] (atLevel :: outputs)
    | key :: rest ->
        if key = 'A' then
            evaluate
                levels
                coords
                currCoord
                rest
                ((coords
                  |> Map.toSeq
                  |> Seq.find (fun (_, c) -> c = currCoord)
                  |> fst)
                 :: output)
                outputs
        else
            let (x, y) = currCoord

            let newCoord =
                match key with
                | '^' -> (x, y - 1)
                | 'v' -> (x, y + 1)
                | '<' -> (x - 1, y)
                | '>' -> (x + 1, y)
                | _ -> failwith "Invalid"

            evaluate levels coords newCoord rest output outputs

let print chars =
    chars |> List.toArray |> String |> printfn "%s"

let printSequence sequence =
    let input = sequence |> Seq.toList
    let levels = [ (dirCoords, (2, 0)) ]
    print input
    let outputs = evaluate levels dirCoords (2, 0) input [] []
    outputs |> List.iter print

let data = loadData ()

let getComplexities2 depth keys =
  let num =
        keys
        |> List.take 3
        |> Array.ofList
        |> String
        |> int64
  let keys = 'A' :: keys
  let minLength =
    keys
    |> List.pairwise
    |> List.map (fun (src, dst) -> getBestPathCost src dst depth)
    |> List.sum
  minLength * num

data
|> List.map (getComplexities2 2)
|> List.sum
|> printfn "Part 1: %d"

data
|> List.map (getComplexities2 25)
|> List.sum
|> printfn "Part 2: %d"
