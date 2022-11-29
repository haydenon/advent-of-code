open System
open System.Collections

type BoardingPass = { Row: bool[]
                      Col: bool[]}

let getTotalNum entries =
  int (Math.Pow(2.0, float entries))

let getValue (keys : bool[]) =
  let bitArr = BitArray(Array.rev keys)
  let intArr: int[] = Array.zeroCreate 1
  bitArr.CopyTo(intArr, 0)
  intArr.[0]

let getRowCol { Row = row; Col = col } =
  (getValue row, getValue col)

let getId rowCount (row, col) =
  row * rowCount + col

let parsePass (text : string) =
  let row = text.Substring(0, 7)
  let col = text.Substring(7, text.Length - 7)
  { Row = Array.map (fun ch -> ch = 'B') (row.ToCharArray())
    Col = Array.map (fun ch -> ch = 'R') (col.ToCharArray()) }

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parsePass

[<EntryPoint>]
let main _ =
  let data = loadData()

  let ids =
    data
    |> Array.map (getRowCol >> (getId 8))

  let min = Array.min ids
  let max = Array.max ids

  let missing =
    seq { min..max }
    |> Seq.find (fun id -> not(Array.contains id ids))

  printfn "Max seat id is %d" max
  printfn "Missing seat id is %d" missing
  0