﻿open System
open System.Collections.Generic

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.map int64

let mixAndPrune secret result =
    let mixResult = secret ^^^ result
    let pruneResult = mixResult % 16777216L
    pruneResult


let getNext num =
    let step1 = num * 64L |> mixAndPrune num
    let step2 = step1 / 32L |> mixAndPrune step1
    let step3 = step2 * 2048L |> mixAndPrune step2
    step3

let getNResult num secret =
    seq { 1..num }
    |> Seq.fold
        (fun acc _ ->
            let next = (getNext (List.head acc))
            next :: acc)
        [ secret ]
    |> List.rev

let data = loadData ()

let secrets = data |> Array.map (getNResult 2000)

secrets
|> Array.map List.last
|> Array.sum
|> printfn "Part 1: %d"

let lastNumbers =
    secrets
    |> Array.map (fun list ->
        list
        |> List.map (fun num ->
            let asStr = num.ToString()
            asStr.Substring(asStr.Length - 1, 1) |> int)
        |> List.toArray)

let prefixMap = Map.empty

let addForPosition (consumed : HashSet<string>) (monkeyNumbers: int array) prefixMap idx =
    let prefix =
        seq { -3 .. 0 }
        |> Seq.map (fun pos ->
            monkeyNumbers[idx + pos]
            - monkeyNumbers[idx + (pos - 1)])
        |> Seq.rev
        |> fun num -> String.Join(",", num )

    let number = monkeyNumbers[idx]

    if not(consumed.Contains prefix) then
      consumed.Add prefix |> ignore
      prefixMap
      |> Map.change prefix (function
          | Some num -> Some(num + number)
          | None -> Some(number))
    else
      prefixMap

let addForMonkey prefixMap (monkeyNumbers: int array) =
    let consumed = new HashSet<string>()
    seq { 4 .. monkeyNumbers.Length - 1 }
    |> Seq.fold (addForPosition consumed monkeyNumbers) prefixMap

let prefixes = lastNumbers |> Array.fold addForMonkey prefixMap

prefixes
|> Map.values
|> Seq.max
|> printfn "Part 2: %d"