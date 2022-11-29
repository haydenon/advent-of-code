open System

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map Int32.Parse
  |> Array.sort
  |> Array.toList

let inc num (tally : Map<int, int>) =
  tally
  |> Map.change num (function Some i -> Some (i + 1) | None -> Some 1)

let rec countDifferences data tally current : Map<int, int> =
  match data with
  | next :: tail ->
    let diff = next - current
    let upd = inc diff tally
    countDifferences tail upd next
  | [] -> tally

let splitList items =
  seq { 0..(List.length items - 1) }
  |> Seq.map (fun i -> List.skip i items)
  |> Seq.toList

let concatNext l1 l2 =
  let l1 = List.skip 1 l1
  l1 @ l2

let rec naiveCountPermutations data current : int64 =
  match data with
  | [] -> int64 1
  | ls ->
    let take =
      match ls with
      | _ :: _ :: _ :: _ -> 3
      | _ -> List.length ls
    let next = List.take take ls
    let tail = List.skip take ls
    splitList next
    |> List.filter (fun ls -> ls.[0] - current <= 3)
    |> List.sumBy (fun ls -> naiveCountPermutations (concatNext ls tail) ls.[0])

let rec splitIntoGroups data groups current previous =
  match data with
  | num :: tail ->
    if num > previous + 1 // Only differences of 1 & 3 in data
    then splitIntoGroups tail (current :: groups) [num] num
    else splitIntoGroups tail groups (num :: current) num
  | [] -> current :: groups

let printValues (tally : Map<int, int>) =
  for (key, value) in Map.toList tally do
    printfn "There were %d jumps of jolt %d" value key
  let threeJumps = tally.[3]
  let oneJumps = tally.[1]
  printfn "Multiplying jumps of 1 & 3, that's %d" (threeJumps * oneJumps)

[<EntryPoint>]
let main _ =
  let data = loadData()
  countDifferences data Map.empty 0
  |> inc 3
  |> printValues

  splitIntoGroups data [] [0] 0
  // |> (fun grp -> printfn "%A" grp; grp)
  |> List.map (fun ls -> naiveCountPermutations (List.skip 1 ls) ls.[0])
  |> List.reduce (*)
  |> printfn "There should be %d different ways of combining adapters"
  0