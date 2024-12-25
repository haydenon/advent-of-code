open System
open System.Collections.Generic
open System.Collections

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let rec parseInputs (lines: string array) locks keys =
        if lines.Length = 0
           || lines |> Array.forall String.IsNullOrWhiteSpace then
            locks, keys
        else
            let nextSplit =
                lines
                |> Array.tryFindIndex String.IsNullOrWhiteSpace
                |> Option.orElse (Some lines.Length)
                |> Option.get

            let content = lines |> Array.take nextSplit
            let isLock = content[0] |> Seq.forall ((=) '#')

            let heights =
                seq { 0 .. content[0].Length - 1 }
                |> Seq.map (fun idx ->
                    (seq { 0 .. content.Length - 1 }
                     |> Seq.filter (fun row -> content[row][idx] = '#')
                     |> Seq.length)
                    - 1)
                |> Seq.toArray

            parseInputs
                (if nextSplit >= lines.Length then
                     [||]
                 else
                     lines |> Array.skip (nextSplit + 1))
                (if isLock then
                     heights :: locks
                 else
                     locks)
                (if isLock then keys else heights :: keys)

    parseInputs text [] []

let checkMatch ((lock: int array), (key: int array)) =
    Array.forall2 (fun k l -> k + l < 6) key lock

let (locks, keys) = loadData ()

List.allPairs locks keys
|> List.filter checkMatch
|> List.length
|> printfn "%d"
