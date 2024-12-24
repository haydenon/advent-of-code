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

let rec getPaths pathMap currKey keys (paths: char list list) =
    match keys with
    | [] -> paths
    | key :: rest ->
        let newPaths: char list list =
            (if key = currKey then
                 paths |> List.map (fun p -> List.append p [ 'A' ])
             else
                 let pathsFromHere = pathMap |> Map.find (currKey, key)

                 List.allPairs paths pathsFromHere
                 |> List.map (fun (toHere, fromHere) -> List.append (List.append toHere fromHere) [ 'A' ]))

        getPaths pathMap key rest newPaths
// let kx, ky = coords |> Map.find key
// let (dx, dy) = kx - x, ky - y
// let newResult = List.append result (List.append (getDirMoves (dx, dy)) [ 'A' ])
// getShortest coords (kx, ky) rest newResult

let getDirMoves (dx, dy) =
    let getMovesInDir dir delta =
        seq { 1..delta }
        |> Seq.map (fun _ -> dir)
        |> Seq.toList

    let xMoves =
        if dx < 0 then
            (getMovesInDir '<' (abs dx))
        else if dx > 0 then
            (getMovesInDir '>' (abs dx))
        else
            []

    let yMoves =
        if dy < 0 then
            (getMovesInDir '^' (abs dy))
        else if dy > 0 then
            (getMovesInDir 'v' (abs dy))
        else
            []

    let getOrder =
        function
        | '>' -> 0
        | '^' -> 1
        | 'v' -> 2
        | '<' -> 3
        | _ -> failwith "Invalid move"

    List.append xMoves yMoves |> List.sortBy getOrder

let rec getShortest coords (x, y) keys result =
    match keys with
    | [] -> result
    | key :: rest ->
        let kx, ky = coords |> Map.find key
        let (dx, dy) = kx - x, ky - y
        let newResult = List.append result (List.append (getDirMoves (dx, dy)) [ 'A' ])
        getShortest coords (kx, ky) rest newResult

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

// New attempt
let dirs = [ '<'; '^'; 'v'; '>' ]

let getBestPath source dest =
    let paths =
        if dirs |> List.contains source
           || dirs |> List.contains dest then
            allDirPaths
        else
            allNumPaths

    let (_, _, results) =
        seq { 1..3 }
        |> Seq.fold
            (fun (src, paths, solutions) _ ->
                ('A',
                 allDirPaths,
                 solutions
                 |> List.collect (fun dst -> getPaths paths src dst [ [] ])))
            (source, paths, [ [ dest ] ])

    results
    |> List.head
    |> fun a -> evaluate [ (dirCoords, (2, 0)) ] dirCoords (2, 0) a [] []
    |> List.last

let cache = Dictionary<(char * char) * int, int64>()

let rec findShortest levels source dest =
    if cache.ContainsKey((source, dest), levels) then
        cache[(source, dest), levels]
    else
        let next = getBestPath source dest

        let result =
            if levels = 0 then
                next |> List.length |> int64
            else
                ('A' :: next)
                |> List.pairwise
                |> List.sumBy (fun (s, d) ->
                    if cache.ContainsKey((source, dest), levels - 1) then
                        cache[(source, dest), levels - 1]
                    else
                        findShortest (levels - 1) s d)

        cache.Add(((source, dest), levels), result)
        result

let data = loadData ()

let getComplexities keys =
    let numMoves = getPaths allNumPaths 'A' keys [ [] ]

    let dir1Moves =
        numMoves
        |> List.collect (fun path -> getPaths allDirPaths 'A' path [ [] ])

    let minLength =
        dir1Moves
        |> List.collect (fun path -> getPaths allDirPaths 'A' path [ [] ])
        |> List.map List.length
        |> List.min

    let num =
        keys
        |> List.take 3
        |> Array.ofList
        |> String
        |> int

    minLength * num

// data
// |> List.map getComplexities
// |> List.sum
// // |> printfn "%A"
// |> printfn "Part 1: %d"

"<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
|> printSequence



//("37" |> Seq.toList)
let level1 = getShortest numberCoords (2, 3) data[4] []
let level2 = getShortest dirCoords (2, 0) level1 []
let level3 = getShortest dirCoords (2, 0) level2 []
level3 |> List.toArray |> String |> printSequence

findShortest 25 '3' '7' |> printfn "%A"
// |> Seq.toList
// |> fun keys -> evaluate [] dirCoords (2,0) keys []
// |> fun keys -> evaluate [] dirCoords (2,0) keys []
// |> print

// data
// |> List.map getSequence
// |> List.map (Array.ofList >> String) //List.length
// |> printfn "%A"

// allDirPaths |> printfn "%A"
