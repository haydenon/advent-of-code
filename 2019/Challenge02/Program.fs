open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text[ 0 ].Split(",") |> Array.map Int32.Parse

let rec runOpCode pos (code: int []) =
    let doOp func pos =
        let l = code[code[pos + 1]]
        let r = code[code[pos + 2]]
        let newPos = code[pos + 3]
        code[newPos] <- func l r
        runOpCode (pos + 4) code

    match code[pos] with
    | 99 -> Some code
    | 1 -> doOp (+) pos
    | 2 -> doOp (*) pos
    | _ -> None

let data = loadData ()

let replaceValues first second (code: int []) =
    let replaced = code |> Array.map id
    replaced[1] <- first
    replaced[2] <- second
    replaced

data
|> replaceValues 12 2
|> runOpCode 0
|> Option.get
|> fun arr -> Array.get arr 0
|> printfn "Part 1: %d"


Seq.allPairs (seq { 0..100 }) (seq { 0..100 })
|> Seq.find ((fun (first,second) -> replaceValues first second data) >> (runOpCode 0) >> (function Some(arr) when arr[0] = 19690720 -> true | _ -> false))
|> (fun (first, second) -> printfn "Part 2: %d%d" first second)