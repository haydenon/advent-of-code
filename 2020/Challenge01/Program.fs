open System

let loadData () =
  let text = System.IO.File.ReadAllLines("./input.txt")
  Array.map Int32.Parse text

let rec findTwoValues (data: int array) value idx =
  let res = data.[idx] + value
  if res = 2020 then Some (value, data.[idx])
  else if res > 2020 then None
  else findTwoValues data value (idx + 1)

let rec findThreeValues (data: int array) value i1 i2 =
  let res = data.[i1] + data.[i2] + value
  if res = 2020 then Some (value, data.[i1], data.[i2])
  else if res > 2020 then None
  else if i2 >= data.Length + 1 then findThreeValues data value (i1 + 1) (i1 + 2)
  else findThreeValues data value i1 (i2 + 1)

[<EntryPoint>]
let main _ =
  let data = loadData() |> Array.sort

  seq{ 0..data.Length }
  |> Seq.tryPick (fun i -> findTwoValues data data.[i] (i + 1))
  |> function
    | Some (x, y) -> printfn "Found values for sum of two: %d & %d -> %d" x y (x * y)
    | None -> ()

  seq{ 0..data.Length }
  |> Seq.tryPick (fun i -> findThreeValues data data.[i] (i + 1) (i + 2))
  |> function
    | Some (x, y, z) -> printfn "Found values for sum of three: %d, %d & %d -> %d" x y z (x * y * z)
    | None -> ()
  0