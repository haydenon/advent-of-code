open System
open System.Linq

type Policy = { Letter: char
                Min: uint8
                Max: uint8 }

let parsePwPolicy (line: string) =
  let createPolicy ch (minMax : uint8 array) =
      { Letter = ch
        Min = minMax.[0]
        Max = minMax.[1] }
  let parsePolicy (text : string) =
    match text.Split(" ") with
    | [| num; ch |] ->
      num.Split('-')
      |> Array.map(Byte.Parse)
      |> createPolicy ch.[0]
    | _ -> failwith "Invalid input"
  match line.Split(":") with
  | [| policy; pw |] ->
    (parsePolicy policy, pw)
  | _ -> failwith "Invalid input"

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parsePwPolicy

let checkPw { Letter = ch; Min = min; Max = max } (pw : string) =
  let count =  byte (pw.Count(fun c -> ch = c))
  count >= min && count <= max

let checkPw2 { Letter = ch; Min = i1; Max = i2 } (pw : string) =
  let i1Present = pw.[int i1] = ch
  let i2Present = pw.[int i2] = ch
  i1Present <> i2Present && (i1Present || i2Present)

[<EntryPoint>]
let main _ =
    let data = loadData()

    data
    |> Array.filter (fun (policy, pw) -> checkPw policy pw)
    |> Array.length
    |> printfn "Part 1: %d"

    data
    |> Array.filter (fun (policy, pw) -> checkPw2 policy pw)
    |> Array.length
    |> printfn "Part 2: %d"
    0