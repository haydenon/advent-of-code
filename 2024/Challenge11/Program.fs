open System
open System.Collections.Generic

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    let line = text[0]
    line.Split(" ") |> Array.map int64 |> Array.toList

let data = loadData ()

let rec getNext value =
    let asStr = string value

    if value = 0L then
        [ 1L ]
    else if asStr.Length % 2 = 0 then
        let half = asStr.Length / 2
        let firstHalf = asStr.Substring(0, half)
        let secondHalf = asStr.Substring(half, half)

        [ int64 firstHalf; int64 secondHalf ]
    else
        [ value * 2024L ]

let rec runRounds (cache: Dictionary<int64 * int, int64>) times value =
    let res = ref 0L
    let key = (value, times)
    let c = cache.TryGetValue(key, res)

    match c with
    | true -> res.Value
    | _ ->
        let next = getNext value

        let nextValue =
            if (times <= 1) then
                next |> List.length |> int64
            else
                next
                |> List.map (fun v -> runRounds cache (times - 1) v)
                |> List.sum

        cache.Add(key, nextValue)
        nextValue

let cache = Dictionary()

data
// |> toLinkedList
// |> runRoundOld 25
|> List.map (runRounds cache 25)
// |> toList []
|> List.sum
|> printfn "Part 1: %d"

data
|> List.map (runRounds cache 75)
|> List.sum
// |> toLinkedList
// |> runWithCaching
|> printfn "Part 2: %d"
