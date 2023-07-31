open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text[ 0 ].Split(",") |> Array.map Int32.Parse

let getOpCode num =
    let opCode = num % 100
    ((num / 10000 % 10 <> 0, num / 1000 % 10 <> 0, num / 100 % 10 <> 0), opCode)

let rec runOpCode input pos (code: int []) =
    let doMathOp func pos (last, mid, first) =
        let l =
            if first then
                code[pos + 1]
            else
                code[code[pos + 1]]

        let r =
            if mid then
                code[pos + 2]
            else
                code[code[pos + 2]]

        let newPos = code[pos + 3]
        code[newPos] <- func l r
        runOpCode input (pos + 4) code

    match getOpCode code[pos] with
    | _, 99 -> Some code
    | modes, 1 -> doMathOp (+) pos modes
    | modes, 2 -> doMathOp (*) pos modes
    | _, 3 ->
        code[code[pos + 1]] <- input
        runOpCode input (pos + 2) code
    | _, 4 ->
        code[code[pos + 1]] |> printfn "%d"
        runOpCode input (pos + 2) code
    | _ -> None

let data = loadData ()

data
|> runOpCode 1 0
// |> Option.get
// |> fun arr -> Array.get arr 0
|> printfn "Part 1: %A"
