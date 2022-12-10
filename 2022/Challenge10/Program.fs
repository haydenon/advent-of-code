open System

type Instruction =
    | Noop
    | Addx of int

let parseInstruction (line: string) =
    if line.StartsWith("noop") then
        Noop
    else
        let parts = line.Split(" ")
        Addx(Int32.Parse parts[1])

let rec run instructions xreg values cycleNum idx remaining =
    if cycleNum = 0 then
        values |> List.rev
    else
        let nextIndex = ((idx + 1) % Array.length instructions)
        let nextCycle = (cycleNum - 1)

        match instructions[idx], remaining with
        | Noop, _ -> run instructions xreg (xreg :: values) nextCycle nextIndex None
        | Addx _, None -> run instructions xreg (xreg :: values) nextCycle idx (Some 0)
        | Addx num, Some remain when remain = 0 ->
            run instructions (xreg + num) (xreg + num :: values) nextCycle nextIndex None
        | Addx _, Some remain -> run instructions xreg (xreg :: values) nextCycle idx (Some(remain - 1))


let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> Array.map parseInstruction


let data = loadData ()

let drawGrid (grid: bool []) =
    let drawLine row =
        let arr = Array.init 40 (fun _ -> '.')
        let rowStart = 40 * row

        seq { 0..39 }
        |> Seq.iter (fun col ->
            if grid[rowStart + col] then
                arr[col] <- '#')

        printfn "%s" (new String(arr))

    seq { 0..5 } |> Seq.iter drawLine

let registerValues = run data 1 [] 240 0 None |> List.toArray

let getIndexValue (reg: int []) idx = reg[idx - 2] * idx

let getPixelValue (registerValues: int []) i =
    let getThreeAround col =
        let first = if col - 1 < 0 then 239 else col - 1
        let third = if col + 1 > 239 then 0 else col + 1
        [ first; col; third ]

    let value = registerValues[i]
    let values = getThreeAround value
    values |> List.contains (i % 40)

[ 20; 60; 100; 140; 180; 220 ]
|> List.sumBy (getIndexValue registerValues)
|> printfn "Part 1: %d"

let grid = Array.init 240 (fun i -> i % 2 = 1)

let values =
    List.init 240 id
    |> List.map (getPixelValue  (Array.append [|1|] registerValues))
    |> List.toArray

printfn "Part 2:"
drawGrid values
