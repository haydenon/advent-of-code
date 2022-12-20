open System

[<AllowNullLiteral>]
type ListNode(value: int64) =
    member val Value = value
    member val Next: ListNode = null with get, set
    member val Prev: ListNode = null with get, set

    override this.ToString() = string value

let createList values =
    let length = (values |> List.length)
    let list = Array.init length (fun _ -> null)
    list[0] <- ListNode values[0]

    for (idx, item) in values |> List.indexed |> List.skip 1 do
        let node = ListNode(item)
        node.Prev <- list[idx - 1]
        list[idx - 1].Next <- node
        list[idx] <- node

    let last = list[length - 1]
    let first = list[0]
    last.Next <- first
    first.Prev <- last

    list

let manipulateList (list: ListNode array) =
    let length = (list |> Array.length)

    let zeroNode = list |> Array.find (fun n -> n.Value = 0)

    let rec createNumList list (node: ListNode) remaining =
        if remaining = 0 then
            list |> List.rev
        else
            createNumList (node.Value :: list) node.Next (remaining - 1)

    let moveItem (node: ListNode) =
        let amount = (abs node.Value) % (int64 length - 1L)
        let forward = node.Value > 0

        if amount <> 0 then
            let prev = node.Prev
            let next = node.Next

            prev.Next <- next
            next.Prev <- prev

            let mutable current = next

            let moveOne () =
                if forward then
                    current <- current.Next
                else
                    current <- current.Prev

            for _ in 1L..amount do
                moveOne ()

            let prev = current.Prev
            prev.Next <- node
            current.Prev <- node
            node.Next <- current
            node.Prev <- prev

    list |> Array.iter moveItem

    createNumList [] zeroNode length

let valueAtIndexes indexes list =
    let length = list |> List.length
    indexes |> List.map (fun i -> list[i % length])

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text
    |> Array.toList
    |> List.map Int64.Parse

let data = loadData ()

data
|> createList
|> manipulateList
|> valueAtIndexes [ 1000; 2000; 3000 ]
|> List.sum
|> printfn "Part 1: %d"


let part2List =
  data
  |> List.map ((*)811589153L)
  |> createList
for _ in 1..9 do
  manipulateList part2List |> ignore

manipulateList part2List
|> valueAtIndexes [ 1000; 2000; 3000 ]
|> List.sum
|> printfn "Part 2: %d"


