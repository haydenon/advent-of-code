open System

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map Int64.Parse

let rec findTwoValues target (data: int64[]) i1 i2 =
  if i2 >= Array.length data then false
  else
    let res = data.[i1] + data.[i2]
    if res = target then true
    else if res > target then false
    else findTwoValues target data i1 (i2 + 1)

let rec findNonMatchingNumber (data : int64[]) count idx =
  let next = data.[idx]
  let priorStart = idx - count
  let priorValues = (Array.sort data.[(priorStart)..(idx - 1)])
  seq { 0..count }
  |> Seq.tryFindIndex (fun i -> findTwoValues next priorValues i (i+1))
  |> function
    | Some _ -> findNonMatchingNumber data count (idx + 1)
    | None ->
      next

let rec findContiguousFirstLast target (data : int64[]) i1 i2 =
  let subset = data.[i1..i2]
  let sum = Array.sum subset
  if sum = target then
    let min = Array.min subset
    let max = Array.max subset
    Some(min + max)
  else if sum < target then findContiguousFirstLast target data i1 (i2 + 1)
  else findContiguousFirstLast target data (i1 + 1) (i1 + 2)

[<EntryPoint>]
let main _ =
  let data = loadData()
  let preamble = 25
  let nonMatchingNumber = findNonMatchingNumber data preamble preamble
  nonMatchingNumber
  |> printfn "Found value that is not sum of two of %d previous numbers %d" preamble
  findContiguousFirstLast nonMatchingNumber data 0 1
  |> function
     | Some arr -> printfn "Min and max sum to: %d" arr
     | None     -> printfn "Not found"
  0