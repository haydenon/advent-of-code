open System
open System.Collections.Generic

let (|Number|_|) (line : string) = if not(String.IsNullOrWhiteSpace line) && Char.IsDigit line.[0] then Some (line |> int) else None

let rec parseData isP2 p1 p2 lines =
  match lines with
  | [] -> (List.rev p1, List.rev p2)
  | Number num :: tail -> parseData isP2 (if isP2 then p1 else num :: p1) (if isP2 then num :: p2 else p2) tail
  | "Player 2:" :: tail -> parseData true p1 p2 tail
  | _ :: tail -> parseData isP2 p1 p2 tail

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.toList
  |> parseData false [] []

let rec playSimple p1 p2 =
  match (p1, p2) with
  | ([], []) -> failwith "Both empty"
  | ([], _) -> p2
  | (_, []) -> p1
  | (n1 :: tail1, n2 :: tail2) ->
    if n1 > n2
    then playSimple (tail1 @ [n1; n2]) tail2
    else playSimple tail1 (tail2 @ [n2; n1])

type ResultMap = Map<int list * int list, bool>

let previousResults = Dictionary<int list * int list, bool>()

let rec playRecursive level p1 p2 previousDecks =
  if previousResults.ContainsKey(p1,p2) then
    let p1Won = previousResults.[(p1,p2)]
    (p1Won, [], [])
  else if Set.contains (p1,p2) previousDecks
  then
    (true, p1, p2)
  else
    match (p1, p2) with
    | ([], []) -> failwith "Both empty"
    | ([], _) -> (false, p1, p2)
    | (_, []) -> (true, p1, p2)
    | (n1 :: tail1, n2 :: tail2) ->
      let prevDecks = Set.add (p1,p2) previousDecks
      let playRecursiveGame = n1 <= List.length tail1 && n2 <= List.length tail2
      let (p1Won, p1, p2) =
        if playRecursiveGame
        then
          let (p1Won, _, _) = playRecursive (level + 1) (List.take n1 tail1) (List.take n2 tail2) Set.empty
          let (p1, p2) = if p1Won then (tail1 @ [n1; n2], tail2) else (tail1, tail2 @ [n2; n1])
          previousResults.[(tail1, tail2)] <- p1Won
          (p1Won, p1, p2)
        else
          let p1Won = n1 > n2
          let (p1, p2) = if p1Won then (tail1 @ [n1; n2], tail2) else (tail1, tail2 @ [n2; n1])
          (p1Won, p1, p2)
      if p1Won
        then playRecursive level p1 p2 prevDecks
        else playRecursive level p1 p2 prevDecks

let getScore deck =
  deck
  |> List.rev
  |> List.mapi (fun i num -> (int64 i + 1L, int64 num))
  |> List.fold (fun acc (ind, num) -> acc + ind * num) 0L

[<EntryPoint>]
let main _ =
  let (p1, p2) = loadData()
  let winningDeck = playSimple p1 p2
  getScore winningDeck
  |> printfn "After normal combat: %d"

  let (p1Won, p1, p2)= playRecursive 0 p1 p2 Set.empty
  let winningDeck = if p1Won then p1 else p2
  getScore winningDeck
  |> printfn "After recursive combat: %d"
  0