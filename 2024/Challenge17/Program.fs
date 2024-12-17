﻿open System
open System.Collections.Generic

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    (text[ 0 ].Split(": ")[1] |> int64,
     text[ 1 ].Split(": ")[1] |> int64,
     text[ 2 ].Split(": ")[1] |> int64,
     (text[ 4 ].Split(": ")[1]).Split(",")
     |> Array.map int32)

let (a, b, c, instructions) = loadData ()

let rec runProgram (instructions: int array) (a, b, c) ip output expectedOutput =
    let getCombo =
        function
        | 0 -> Some 0L
        | 1 -> Some 1L
        | 2 -> Some 2L
        | 3 -> Some 3L
        | 4 -> Some a
        | 5 -> Some b
        | 6 -> Some c
        | _ -> None

    if ip >= instructions.Length then
        Some(output |> List.rev)
    else
        let op = instructions[ip + 1]

        match instructions[ip] with
        | 0 ->
            match getCombo op with
            | Some combo ->
                let denom = Math.Pow(2, float combo)
                let newA = float a / denom |> int
                runProgram instructions (newA, b, c) (ip + 2) output expectedOutput
            | None -> None
        | 1 -> runProgram instructions (a, b ^^^ op, c) (ip + 2) output expectedOutput
        | 2 ->
            match getCombo op with
            | Some combo -> runProgram instructions (a, (combo) % 8L, c) (ip + 2) output expectedOutput
            | None -> None
        | 3 ->
            let nextIp = if a <> 0 then op else ip + 2
            runProgram instructions (a, b, c) nextIp output expectedOutput
        | 4 -> runProgram instructions (a, b ^^^ c, c) (ip + 2) output expectedOutput
        | 5 ->
            match getCombo op with
            | Some combo ->
                let res = (combo) % 8L |> int

                match expectedOutput with
                | Some (out :: rest) when out = res ->
                    runProgram instructions (a, b, c) (ip + 2) (res :: output) (Some rest)
                | None -> runProgram instructions (a, b, c) (ip + 2) (res :: output) None
                | _ -> None
            | None -> None
        | 6 ->
            match getCombo op with
            | Some combo ->
                let denom = Math.Pow(2, float combo)
                let newB = float a / denom |> int
                runProgram instructions (a, newB, c) (ip + 2) output expectedOutput
            | None -> None
        | 7 ->
            match getCombo op with
            | Some combo ->
                let denom = Math.Pow(2, float combo)
                let newC = float a / denom |> int
                runProgram instructions (a, b, newC) (ip + 2) output expectedOutput
            | None -> None
        | _ -> None

let res =
    (runProgram instructions (a, b, c) 0 [] None)
    |> Option.get

String.Join(",", res) |> printfn "Part 1: %s"

let instList = instructions |> Array.toList

seq { 1L .. Int64.MaxValue }
|> Seq.pick (fun a ->
    match runProgram instructions (a, b, c) 0 [] (Some instList) with
    | Some output when output = instList -> Some(a)
    | _ -> None)
|> printfn "Part 2: %d"
