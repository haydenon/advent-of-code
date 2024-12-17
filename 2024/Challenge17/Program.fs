open System
open System.Collections.Generic

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    (text[ 0 ].Split(": ")[1] |> int32,
     text[ 1 ].Split(": ")[1] |> int32,
     text[ 2 ].Split(": ")[1] |> int32,
     (text[ 4 ].Split(": ")[1]).Split(",")
     |> Array.map int32)

let (a, b, c, instructions) = loadData ()

let rec runProgram (instructions: int array) (a, b, c) ip output =
    let getCombo =
        function
        | 0 -> 0
        | 1 -> 1
        | 2 -> 2
        | 3 -> 3
        | 4 -> a
        | 5 -> b
        | 6 -> c
        | _ -> failwith "Invalid"

    if ip >= instructions.Length then
        output |> List.rev
    else
        let op = instructions[ip + 1]

        match instructions[ip] with
        | 0 ->
            let denom = Math.Pow(2, getCombo op)
            let newA = float a / denom |> int
            runProgram instructions (newA, b, c) (ip + 2) output
        | 1 -> runProgram instructions (a, b ^^^ op, c) (ip + 2) output
        | 2 -> runProgram instructions (a, (getCombo op) % 8, c) (ip + 2) output
        | 3 ->
            let nextIp = if a <> 0 then op else ip + 2
            runProgram instructions (a, b, c) nextIp output
        | 4 -> runProgram instructions (a, b ^^^ c, c) (ip + 2) output
        | 5 -> runProgram instructions (a, b, c) (ip + 2) ((getCombo op) % 8 :: output)
        | 6 ->
            let denom = Math.Pow(2, getCombo op)
            let newB = float a / denom |> int
            runProgram instructions (a, newB, c) (ip + 2) output
        | 7 ->
            let denom = Math.Pow(2, getCombo op)
            let newC = float a / denom |> int
            runProgram instructions (a, b, newC) (ip + 2) output
        | _ -> failwith "Invalid"



let res = runProgram instructions (a, b, c) 0 []
String.Join(",", res) |> printfn "%s"
