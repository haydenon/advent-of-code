open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    let line = text[0]
    line.Split(" ") |> Array.map int64 |> Array.toList

let data = loadData ()

[<AllowNullLiteral>]
type ListNode(value: int64) =
    member val Value = value with get, set
    member val Next: ListNode = null with get, set
    member val Start: ListNode = null with get, set

let toLinkedList values =
    let start = ListNode(List.head values)
    start.Start <- start
    let mutable curr = start

    for next in List.skip 1 values do
        let nextNode = ListNode(next)
        nextNode.Start <- start
        curr.Next <- nextNode
        curr <- nextNode

    start

let rec toList values (list: ListNode) =
    let newValues = list.Value :: values

    if list.Next = null then
        newValues |> List.rev
    else
        toList newValues list.Next


let rec runRound times (list: ListNode) =
    let next = list.Next
    let value = list.Value
    let asStr = string value

    if value = 0L then
        list.Value <- 1
    else if asStr.Length % 2 = 0 then
        let half = asStr.Length / 2
        let firstHalf = asStr.Substring(0, half)
        let secondHalf = asStr.Substring(half, half)
        list.Value <- int64 firstHalf
        let newVal = ListNode(int64 secondHalf)
        newVal.Next <- list.Next
        newVal.Start <- list.Start
        list.Next <- newVal
    else
        list.Value <- value * 2024L

    if next = null then
        let newTimes = times - 1
        if newTimes = 0 then
          list.Start
        else runRound newTimes list.Start
    else
        runRound times next

data
|> toLinkedList
|> runRound 25
|> toList []
|> List.length
|> printfn "Part 1: %d"
