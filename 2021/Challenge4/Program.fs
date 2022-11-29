open System

let addBoard boards current =
  if current |> List.length = 0
  then
    boards
  else
    let newBoard = current |> List.rev |> List.map List.toArray |> List.toArray
    newBoard :: boards

let parseBoardLine (boardLine : string) =
  boardLine.Split()
  |> Array.filter (String.IsNullOrWhiteSpace >> not)
  |> Array.map Int32.Parse
  |> List.ofArray

let rec parseBoards text boards current =
  match text with
  | [] ->
    addBoard boards current
    |> List.rev
  | "" :: tail ->
    let newBoards = (addBoard boards current)
    parseBoards tail newBoards []
  | boardLine :: tail ->
    let newCurrent = parseBoardLine boardLine :: current
    parseBoards tail boards newCurrent

let loadData () =
  let text = System.IO.File.ReadAllLines("./input.txt")
  let numbers =
    text
    |> Array.head
    |> (fun (line : string) -> line.Split(","))
    |> Array.map Int32.Parse
  let rest = text |> Array.skip 1 |> Array.toList
  (numbers |> Array.toList, parseBoards rest [] [])


let hasWon numbers board =
  let boardSize = board |> Array.length
  let rowFull (board : int[][]) i =
    Array.TrueForAll(board[i], (fun n -> List.contains n numbers))
  let colFull (board : int[][]) i =
    Array.TrueForAll(board, (fun row -> List.contains row[i] numbers))
  seq { 0..boardSize - 1 }
  |> Seq.exists (fun i ->  rowFull board i || colFull board i)

let getWinningBoard numbers boards =
  boards
  |> List.tryFind (hasWon numbers)

let rec getWinningBoardAndNumbers boards called remaining =
  match (getWinningBoard called boards, remaining) with
  | (Some winningBoard, _) -> (winningBoard, called |> List.rev)
  | (None, next :: tail) ->
    getWinningBoardAndNumbers boards (next :: called) tail
  | (_ , []) -> failwith "No winning board"

let rec getLastWinningBoardAndNumbers boards called remaining =
  let remainingBoards = boards |> List.filter (hasWon called >> not)
  match (remainingBoards, remaining) with
  | ([lastBoard], _) -> getWinningBoardAndNumbers [lastBoard] called remaining
  | ([] , _) | (_, []) -> failwith "No winning board"
  | ( _ , next :: tail) ->
    getLastWinningBoardAndNumbers boards (next :: called) tail

let sumOfUnmarked (board : int[][]) called =
  let isUnmarked num = called |> List.contains num |> not
  board
  |> Array.collect (fun row -> row |> Array.filter isUnmarked)
  |> Array.sum

let (numbers, boards) = loadData()

let firstCalled = numbers |> List.take 1
let remaining = numbers |> List.skip 1
let (winningBoard, called) = getWinningBoardAndNumbers boards firstCalled remaining

let unmarked = sumOfUnmarked winningBoard called
let lastCalled = called |> List.last
printfn "Part 1: %d" (unmarked * lastCalled)

let (lastWinning, lastWinningCalled) = getLastWinningBoardAndNumbers boards firstCalled remaining

let unmarkedLast = sumOfUnmarked lastWinning lastWinningCalled
let lastCalledLast = lastWinningCalled |> List.last
printfn "Part 2: %d" (unmarkedLast * lastCalledLast)