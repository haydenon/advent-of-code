open System

let loadData () =
  let parseInput (text : string) =
    text.Split(",")
    |> Array.map Int32.Parse
  parseInput (System.IO.File.ReadAllLines("./input.txt").[0])

let rec run endIdx lastNum numberIdx idx =
  if idx % 10000 = 0 then
    Console.SetCursorPosition(0, Console.CursorTop)
    printf "%.01f%%" ((float idx / float endIdx) * 100.0)
  if idx > endIdx
  then
    Console.SetCursorPosition(0, Console.CursorTop)
    lastNum
  else
    let nextNum =
      match Map.tryFind lastNum numberIdx with
      | Some lastNumIdx ->
        idx - lastNumIdx - 1
      | None -> 0
    let nextMap = Map.change lastNum (fun _ -> Some (idx - 1)) numberIdx
    run endIdx nextNum nextMap (idx + 1)

[<EntryPoint>]
let main _ =
  let data = loadData()
  let initMap =
    data
    |> Array.take (data.Length - 1)
    |> Array.mapi (fun idx num -> (num, idx))
    |> Map.ofArray
  let endIdx = 2020
  run (endIdx - 1) (Array.last data) initMap (data.Length)
  |> printfn "The number spoken at turn %d is: %d" endIdx

  let endIdx = 30000000
  run (endIdx - 1) (Array.last data) initMap (data.Length)
  |> printfn "The number spoken at turn %d is: %d" endIdx
  0