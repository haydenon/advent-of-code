open System

let loadData () =
  System.IO.File.ReadAllLines("./input.txt").[0].ToCharArray()
  |> Array.map (string >> int32)
  |> Array.toList

[<AllowNullLiteral>]
type ListNode(value : int32) =
  member val Value = value
  member val Next : ListNode = null with get, set
  member val Prev : ListNode = null with get, set

let toDoublyLinkedList values =
  let start = ListNode(List.head values)
  let mutable curr = start
  for next in List.skip 1 values do
    let nextNode = ListNode(next)
    nextNode.Prev <- curr
    curr.Next <- nextNode
    curr <- nextNode
  curr.Next <- start
  start.Prev <- curr
  start

let getNValues n (list : ListNode) =
  let values = Array.zeroCreate n
  let mutable curr = list
  for i in 0..n - 1 do
    values.[i] <- curr.Value
    curr <- curr.Next
  values

let findNode pred (values : ListNode) : ListNode =
  let mutable curr = values
  while curr.Value |> pred |> not do
    curr <- curr.Prev
  curr


let getDestinationLabel max value exclusion =
  let mutable destLabel = (value - 2 + max) % max + 1
  while Array.contains destLabel exclusion do destLabel <- ((destLabel - 2 + max) % max + 1)
  destLabel

let removeNAfter n (node : ListNode) =
  let start = node.Next
  let mutable endNode = start
  for _ in 1..n do
    endNode <- endNode.Next
  endNode.Prev.Next <- null
  start.Prev <- null
  node.Next <- endNode
  endNode.Prev <- node
  start

let addNodesAfter (toInsert : ListNode) (target : ListNode) =
  let after = target.Next
  target.Next <- toInsert
  toInsert.Prev <- target
  let mutable curr = toInsert
  while not(isNull curr.Next) do
    curr <- curr.Next
  curr.Next <- after
  after.Prev <- curr

let rotate rotate values =
  let len = List.length values
  let rotate = (rotate + len) % len
  if rotate = 0
  then values
  else
    List.skip (len - rotate) values @ List.take (len - rotate) values

let linkListToArray count (node : ListNode) =
  let values = Array.zeroCreate count
  let mutable currNode = node
  for i in 0..count - 1 do
    values.[i] <- currNode.Value
    currNode <- currNode.Next
  values

let getStartOfList currentIndex (node : ListNode) =
  let mutable idx = currentIndex
  let mutable curr = node
  while idx > 0 do
    curr <- curr.Prev
    idx <- idx - 1
  curr

type NodeLookup = Map<int, ListNode>

let getNodeLookup count (start : ListNode) : NodeLookup =
  let values: (int * ListNode) array = Array.zeroCreate count
  let mutable curr = start
  for i in 0..count - 1 do
    values.[i] <- (curr.Value, curr)
    curr <- curr.Next
  values
  |> Map.ofArray

let rec play n exitAfter max (lookup : NodeLookup) (values : ListNode) current =
  // if n % 10000 = 0 then
    // Console.SetCursorPosition(0, Console.CursorTop)
    // printf "%.01f%%" ((float n / float exitAfter) * 100.0)
  if n > exitAfter then
    // Console.SetCursorPosition(0, Console.CursorTop)
    values
  else
    let moving = getNValues 3 values.Next
    let destLabel = getDestinationLabel max values.Value moving
    let target = lookup.[destLabel]
    let movingNodes = removeNAfter 3 values
    addNodesAfter movingNodes target

    play (n + 1) exitAfter max lookup values.Next ((current + 1) % max)

let getValuesAfter1 values =
  let values = List.ofArray values
  let ind = List.findIndex ((=) 1) values
  let afterOne =
    rotate -ind values
    |> List.skip 1
    |> List.map string
  String.Join("", afterOne)

let getMillionCups values =
  let max = List.max values
  let remainingCups = seq { max + 1..1_000_000} |> Seq.toList
  values @ remainingCups

let getTwoAfterOne (lookup : NodeLookup) =
  let firstNext = lookup.[1].Next.Value
  let secondNext = lookup.[1].Next.Next.Value
  (firstNext, secondNext)

[<EntryPoint>]
let main _ =
  let data = loadData()

  let moves = 100
  let count = (data.Length)
  let playOneData = toDoublyLinkedList data
  let lookup = getNodeLookup count playOneData
  play 1 moves count lookup playOneData 0
  |> linkListToArray count
  |> getValuesAfter1
  |> printfn "The labels after cup 1 after %d moves is %s" moves

  let millionCups = getMillionCups data
  let count = 1_000_000
  let playDataTwo = toDoublyLinkedList millionCups
  let lookup = getNodeLookup count playDataTwo
  let moves = 10_000_000
  play 1 moves count lookup playDataTwo 0 |> ignore
  let (firstAfter, secondAfter) = getTwoAfterOne lookup

  printfn "The first two after cup 1 are %d & %d. Multipled that's: %d" firstAfter secondAfter (int64 firstAfter * int64 secondAfter)
  0