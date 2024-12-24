open System
open System.Collections.Generic
open System.Collections

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let parseInput (input: string) =
        let parts = input.Split(": ")
        (parts[0], parts[1] = "1")

    let parseOperation (operation: string) =
        let parts = operation.Split(" -> ")
        let opParts = parts[ 0 ].Split(" ")
        (opParts[0], opParts[1], opParts[2], parts[1])

    let splitIndex = text |> Array.findIndex String.IsNullOrWhiteSpace

    let inputs =
        text
        |> Array.take splitIndex
        |> Array.map parseInput

    let operations =
        text
        |> Array.skip (splitIndex + 1)
        |> Array.map parseOperation

    (inputs, operations)


let bitArrayToInt64 (values: bool array) =
    let numArr: byte array = BitConverter.GetBytes 0L
    let bitArray = BitArray(numArr)

    for (idx, value) in values |> Array.indexed do
        if value then bitArray.Set(idx, value)

    let byteArray: byte [] = Array.zeroCreate 8
    bitArray.CopyTo(byteArray, 0)
    BitConverter.ToInt64(byteArray, 0)

let evaluateZNumber (inputs: (string * bool) array) (operations: (string * string * string * string) array) =
    let cache = Dictionary<string, bool>()

    for (name, value) in inputs do
        cache.Add(name, value)

    let valueToOperation =
        operations
        |> Array.fold (fun acc (left, op, right, target) -> acc |> Map.add target (left, op, right)) Map.empty

    let rec evaluateValue name =
        if cache.ContainsKey name then
            cache[name]
        else
            let (left, op, right) = valueToOperation |> Map.find name
            let leftVal = evaluateValue left
            let rightVal = evaluateValue right

            let result =
                match op with
                | "AND" -> leftVal && rightVal
                | "OR" -> leftVal || rightVal
                | "XOR" -> (leftVal || rightVal) && not (leftVal && rightVal)
                | _ -> failwith "Invalid"

            cache.Add(name, result)
            result

    let values =
        valueToOperation
        |> Map.keys
        |> Seq.filter (fun k -> k.StartsWith("z"))
        |> Seq.map (fun name -> (name.Substring(1, 2) |> int, evaluateValue name))
        |> Map.ofSeq

    let maxVal = values.Keys |> Seq.max

    let arr =
        seq { 0..maxVal }
        |> Seq.map (fun idx -> values |> Map.find idx)
        |> Seq.toArray

    bitArrayToInt64 arr

let (inputs, operations) = loadData ()

evaluateZNumber inputs operations |> printfn "%d"
