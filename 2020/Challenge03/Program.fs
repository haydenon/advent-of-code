open System

let parseLine (text : string) : bool[] =
  text.ToCharArray()
  |> Array.map (fun ch -> ch = '#')

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseLine

let checkRow movement cols (data : bool[][]) step row =
  let rowData = data.[row]
  let col = (step * movement)
  let col = col % cols
  rowData.[col]

let check cols (data : bool[][]) (right, down)  =
  [| 0..down..(data.Length - 1) |]
  |> Array.filter (fun row -> checkRow right cols data (row / down) row)
  |> Array.length

[<EntryPoint>]
let main _ =
  let data = loadData()
  let cols = data.[0].Length
  let treeHits = check cols data (3, 1 )
  printfn "Hit %d trees while moving 3 right" treeHits
  let combination =
    [(1,1);(3,1);(5,1);(7,1);(1,2)]
    |> List.map ((check cols data) >> uint64)
    |> List.reduce((*))
  printfn "Hit %d trees for all slope combinations" combination
  0