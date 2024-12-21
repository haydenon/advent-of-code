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

// let getDirMoves (dx, dy) =
//     let getMovesInDir dir delta =
//         seq { 1..delta }
//         |> Seq.map (fun _ -> dir)
//         |> Seq.toList

//     let xMoves =
//         if dx < 0 then
//             (getMovesInDir '<' (abs dx))
//         else if dx > 0 then
//             (getMovesInDir '>' (abs dx))
//         else
//             []

//     let yMoves =
//         if dy < 0 then
//             (getMovesInDir '^' (abs dy))
//         else if dy > 0 then
//             (getMovesInDir 'v' (abs dy))
//         else
//             []

//     let getOrder =
//         function
//         | '>'
//         | '^' -> 0
//         | 'v'
//         | '<' -> 1
//         | _ -> failwith "Invalid move"

//     List.append xMoves yMoves |> List.sortBy getOrder

// let rec getShortest coords (x, y) keys result =
//     match keys with
//     | [] -> result
//     | key :: rest ->
//         let kx, ky = coords |> Map.find key
//         let (dx, dy) = kx - x, ky - y
//         let newResult = List.append result (List.append (getDirMoves (dx, dy)) [ 'A' ])
//         getShortest coords (kx, ky) rest newResult

let data = loadData ()

let getComplexities keys =
    let numMoves = getPaths allNumPaths 'A' keys [[]]
    let dir1Moves = numMoves |> List.collect (fun path ->  getPaths allDirPaths 'A' path  [[]])
    let minLength =
      dir1Moves |> List.collect (fun path ->  getPaths allDirPaths 'A' path  [[]])
      |> List.map List.length
      |> List.min
    let num = keys |> List.take 3 |> Array.ofList |> String |> int
    minLength * num

data
|> List.map getComplexities
|> List.sum
// |> printfn "%A"
|> printfn "Part 1: %d"

// data
// |> List.map getSequence
// |> List.map (Array.ofList >> String) //List.length
// |> printfn "%A"

// allDirPaths |> printfn "%A"
