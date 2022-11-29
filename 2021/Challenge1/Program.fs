open System

let loadData () =
  let text = System.IO.File.ReadAllLines("./input.txt")
  Array.map Int32.Parse text
  |> List.ofArray

let rec countDepthIncreases data previous count =
  match data with
  | [] -> count
  | next :: tail when next > previous -> countDepthIncreases tail next (count + 1)
  | next :: tail -> countDepthIncreases tail next count

let previousThree previous =
  match previous with
  | a :: b :: c :: _ -> a + b + c
  | _ -> failwith "Invalid previous"

let rec countSlidingDepthIncreases data previous count =
  match data with
  | [] -> count
  | next :: tail ->
    let nextPrevious = next :: previous
    let newCount = if previousThree nextPrevious > previousThree previous then count + 1 else count
    countSlidingDepthIncreases tail nextPrevious newCount

let data = loadData()

countDepthIncreases data 0 -1
|> printfn "First part: %d"

let start = data |> List.take 3 |> List.rev
let newData = data |> List.skip 3
countSlidingDepthIncreases newData start 0
|> printfn "Second part: %d"