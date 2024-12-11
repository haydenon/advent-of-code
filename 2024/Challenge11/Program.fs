open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    let line = text[0]
    line.Split(" ") |> Array.map int64 |> Array.toList

let data = loadData ()

[<AllowNullLiteral>]
type ListNode(value: int64) =
    member val Count = 0L with get, set
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

    start.Count <- values |> List.length |> int64

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
        let start = list.Start
        newVal.Start <- start
        start.Count <- start.Count + 1L
        list.Next <- newVal
    else
        list.Value <- value * 2024L

    if next = null then
        let newTimes = times - 1

        if newTimes = 0 then
            list.Start
        else
            runRound newTimes list.Start
    else
        runRound times next

let runWithCaching list =
    let oneRound = runRound 25 list
    // let cached = oneRound |>
    let cacheValue (cache: Map<int64, (ListNode * int64)>) num =
        match cache |> Map.tryFind num with
        | Some _ -> cache
        | None ->
            let res = runRound 25 ([ num ] |> toLinkedList)
            let value = (res, res |> toList [] |> List.length |> int64)
            cache |> Map.add num value

    let getValue (cache: Map<int64, (ListNode * int64)>) num =
        match cache |> Map.tryFind num with
        | Some (list, _) -> list
        | None -> runRound 25 ([ num ] |> toLinkedList)

    let roundOneList = oneRound |> toList []
    let cache = roundOneList |> List.fold cacheValue Map.empty
    let roundTwo = roundOneList |> List.map (getValue cache)

    let mutable i = 0

    roundTwo
    |> List.fold
        (fun (cache, count: int64) value ->
            i <- i + 1
            if i % 10000 = 0 then printfn "%d" i
            let mutable curr = value
            let mutable cacheVal = cache
            let mutable currCount = count

            while curr <> null do
                cacheVal <- cacheValue cacheVal curr.Value

                currCount <-
                    currCount
                    + (cacheVal |> Map.find curr.Value |> snd)

                curr <- curr.Next

            (cacheVal, currCount))
        (cache, 0)
        |> snd

data
|> toLinkedList
|> runRound 25
|> toList []
|> List.length
|> printfn "Part 1: %d"

data
|> toLinkedList
|> runWithCaching
|> printfn "Part 2: %d"
