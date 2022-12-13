open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Packet =
    | Num of int
    | List of Packet list

let parsePacket (str: string) =
    let result =
        Regex.Split(str, @"(\[|\]|\d+|,)")
        |> Array.filter (String.IsNullOrWhiteSpace >> not)
        |> Array.toList

    let rec parseItems current (items: string list) =
        match items with
        | "[" :: rest ->
            let children, rest = parseItems [] rest
            parseItems (children :: current) rest
        | "," :: rest -> parseItems current rest
        | "]" :: rest -> (List(current |> List.rev), rest)
        | num :: rest -> parseItems ((Int32.Parse num |> Num) :: current) rest
        | [] -> List(current |> List.rev), []

    match parseItems [] result with
    | List (values), _ -> List.head values
    | _ -> failwith ""

let rec parsePacketPairs pairs (lines: string list) =
    match lines with
    | blank :: rest when blank |> String.IsNullOrWhiteSpace -> parsePacketPairs pairs rest
    | first :: second :: rest ->
        let pairs = (parsePacket first, parsePacket second) :: pairs
        parsePacketPairs pairs rest
    | _ -> pairs |> List.rev


let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> Array.toList |> parsePacketPairs []

let rec checkPair (item1, item2) =
    match (item1, item2) with
    | (Num a, Num b) -> if a = b then None else Some(a < b)
    | (List _, Num _) -> checkPair (item1, List([ item2 ]))
    | (Num _, List _) -> checkPair (List([ item1 ]), item2)
    | (List a, List b) ->
        let aLength = List.length a
        let bLength = List.length b

        let aShorter = aLength < bLength
        let bShorter = aLength > bLength

        let a, b =
            if aShorter then
                (a, List.take aLength b)
            else
                (List.take bLength a, b)

        let res =
            List.zip a b
            |> List.fold
                (fun curr pair ->
                    match curr with
                    | None -> checkPair pair
                    | curr -> curr)
                None


        match res, aShorter with
        | None, true -> Some true
        | None, false -> if bShorter then Some false else None
        | res, _ -> res

let compare a b =
    match checkPair (a, b) with
    | Some true -> -1
    | Some false -> 1
    | None -> 0


let data = loadData ()

data
|> List.map (checkPair >> Option.get) //defaultValue false)
|> List.indexed
|> List.filter snd
|> List.map (fst >> ((+) 1))
|> List.sum
|> printfn "Part 1: %d"

let unnested = data |> List.collect (fun (a, b) -> [ a; b ])

let divA = List([ List([ Num 2 ]) ])
let divB = List([ List([ Num 6 ]) ])

let sorted =
    (divA :: divB :: unnested)
    |> List.sortWith compare

let aInd = (List.findIndex (fun p -> p = divA) sorted) + 1
let bInd = (List.findIndex (fun p -> p = divB) sorted) + 1
printfn "Part 2: %d" (aInd * bInd)
