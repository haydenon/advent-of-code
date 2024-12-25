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

let bitArrayToDisplay (values: bool array) =
    let numArr: byte array = BitConverter.GetBytes 0L
    let bitArray = BitArray(numArr)

    for (idx, value) in values |> Array.indexed do
        if value then bitArray.Set(idx, value)

    String.Join(
        "",
        seq { 0..63 }
        |> Seq.map (fun idx ->
            if bitArray.Get(63 - idx) then
                "1"
            else
                "0")
    )

let numberToDisplay (value: int64) =
    let numArr: byte array = BitConverter.GetBytes value
    let bitArray = BitArray(numArr)

    String.Join(
        "",
        seq { 0..63 }
        |> Seq.map (fun idx ->
            if bitArray.Get(63 - idx) then
                "1"
            else
                "0")
    )

let evaluate (inputs: (string * bool) array) (operations: (string * string * string * string) array) =
    let evaluateNumber
        (prefix: string)
        (inputs: (string * bool) array)
        (valueToOperation: Map<string, string * string * string>)
        =
        let cache = Dictionary<string, Option<bool>>()
        let visited = HashSet<string>()

        for (name, value) in inputs do
            cache.Add(name, Some(value))

        let rec evaluateValue name =
            if cache.ContainsKey name then
                cache[name]
            else if visited.Contains name then
                None
            else
                visited.Add(name) |> ignore
                let (left, op, right) = valueToOperation |> Map.find name
                let leftVal = evaluateValue left
                let rightVal = evaluateValue right

                let result =
                    match leftVal, op, rightVal with
                    | Some (left), "AND", Some (right) -> Some(left && right)
                    | Some (left), "OR", Some (right) -> Some(left || right)
                    | Some (left), "XOR", Some (right) -> Some((left || right) && not (left && right))
                    | _ -> None

                cache.Add(name, result)
                result

        let values =
            valueToOperation
            |> Map.keys
            |> Seq.append (inputs |> Array.map fst)
            |> Seq.filter (fun k -> k.StartsWith(prefix))
            |> Seq.map (fun name -> (name.Substring(1, 2) |> int, evaluateValue name))
            |> Map.ofSeq

        let maxVal = values.Keys |> Seq.max

        let arr =
            seq { 0..maxVal }
            |> Seq.fold
                (fun acc idx ->
                    match acc, values |> Map.find idx with
                    | None, _
                    | _, None -> None
                    | Some acc, Some v -> Some(v :: acc))
                (Some [])
            |> Option.map (List.rev >> List.toArray)

        arr |> Option.map (fun a -> (a, maxVal))

    let valueToOperation =
        operations
        |> Array.fold (fun acc (left, op, right, target) -> acc |> Map.add target (left, op, right)) Map.empty

    // let getNumberForPrefix  =

    let arr, maxVal =
        evaluateNumber "z" inputs valueToOperation
        |> Option.get

    let originalZ = bitArrayToInt64 arr

    let xArr, _ =
        evaluateNumber "x" inputs valueToOperation
        |> Option.get

    let yArr, _ =
        evaluateNumber "y" inputs valueToOperation
        |> Option.get

    let expected =
        (bitArrayToInt64 xArr + bitArrayToInt64 yArr)
        |> numberToDisplay

    let actual = bitArrayToDisplay arr

    let rec getGatesUsed name =
        match valueToOperation |> Map.tryFind name with
        | None -> Set.empty
        | Some (left, _, right) ->
            (getGatesUsed left + getGatesUsed right)
            |> Set.add name

    let mismatched =
        seq { 0..63 }
        |> Seq.filter (fun idx -> actual[63 - idx] <> expected[63 - idx])
        |> Seq.toArray

    let allGates = (valueToOperation |> Map.keys |> Set.ofSeq)

    let definitelyOkValues =
        seq { 0..maxVal }
        |> Seq.fold
            (fun acc idx ->
                if mismatched |> Array.contains idx then
                    acc - getGatesUsed (("z" + (sprintf "%02d" idx)))
                else
                    acc)
            allGates

    let addPairings (pairs: (string * string) list) pairings =
        let sorted =
            pairs
            |> List.map (fun (a, b) ->
                if a.CompareTo(b) < 0 then
                    (a, b)
                else
                    (b, a))
            |> List.sortBy fst

        pairings |> Set.add sorted

    let rec getPairings pairs previous count (values: Set<string>) output =
        if count = 0 then
            addPairings pairs output
        else if count % 2 = 1 then

            values
            |> Set.fold
                (fun acc v ->
                    let thisValue = ([ v ] |> Set.ofList)

                    acc
                    |> Set.union (getPairings ((v, previous) :: pairs) "" (count - 1) (values - thisValue) output))
                output
        else
            values
            |> Set.fold
                (fun acc v ->
                    acc
                    + getPairings pairs v (count - 1) (values - ([ v ] |> Set.ofList)) output)
                output

    let sort (a: string, b: string) =
        if a.CompareTo(b) < 0 then
            (a, b)
        else
            (b, a)

    let sortList (pairs: (string * string) list) = pairs |> List.sortBy fst

    let invalidPairings = HashSet<(string * string) list>()
    let invalidPairs = HashSet<(string * string)>()

    let rec findPairings
        pairs
        previous
        count
        (values: Set<string>)
        (mismatched: int array)
        (valueToOperation: Map<string, string * string * string>)
        : Option<(string * string) list> =
        if count = 0 then
            if mismatched.Length = 0 then
                Some(pairs)
            else
                None
        else if count % 2 = 1 then
            values
            |> Seq.tryPick (fun v ->
                let sorted = (sort (previous, v))

                // if ((sorted |> fst = "z00" && sorted |> snd = "z05")
                //     || (sorted |> fst = "z01" && sorted |> snd = "z02"))
                //    && not(pairs.IsEmpty) then
                //     printfn ""
                // if ((sorted |> fst = "z00" && sorted |> snd = "z05")
                //     || (sorted |> fst = "z01" && sorted |> snd = "z02"))
                //    && pairs.IsEmpty then
                //     printfn ""

                if
                    invalidPairs.Contains sorted
                    || invalidPairings.Contains(sortList (sorted :: pairs))
                then
                    None
                else
                    let aGate = valueToOperation |> Map.find previous
                    let bGate = valueToOperation |> Map.find v

                    let replaced =
                        valueToOperation
                        |> Map.add previous bGate
                        |> Map.add v aGate

                    match evaluateNumber "z" inputs replaced with
                    | Some (res, _) ->
                        let result = bitArrayToDisplay res


                        if (seq { 0..maxVal }
                            |> Seq.exists (fun idx ->
                                result[63 - idx] <> expected[63 - idx]
                                && not (mismatched |> Array.contains idx)))
                           || (seq { 0..maxVal }
                               |> Seq.filter (fun idx -> result[63 - idx] <> expected[63 - idx])
                               |> Seq.length) >= mismatched.Length then
                            invalidPairs.Add sorted |> ignore

                            invalidPairings.Add(sortList (sorted :: pairs))
                            |> ignore

                            None
                        else
                            let newMismatched =
                                seq { 0..63 }
                                |> Seq.filter (fun idx -> result[63 - idx] <> expected[63 - idx])
                                |> Seq.toArray

                            let result =
                                findPairings
                                    (sorted :: pairs)
                                    ""
                                    (count - 1)
                                    (values - ([ v ] |> Set.ofList))
                                    newMismatched
                                    replaced

                            match result with
                            | Some a -> Some a
                            | None ->
                                invalidPairings.Add(sortList (sorted :: pairs))
                                |> ignore

                                None
                    | None ->
                        invalidPairs.Add sorted |> ignore
                        invalidPairings.Add(sortList (sorted :: pairs))
                        |> ignore

                        None)
        else
            values
            |> Seq.tryPick (fun v ->
                if count = 8 then printfn "%s" v
                findPairings pairs v (count - 1) (values - ([ v ] |> Set.ofList)) mismatched valueToOperation)
    // acc
    // + getPairings pairs v (count - 1) (values - ([ v ] |> Set.ofList)) output)
    // Some()
    // values
    // |> Set.fold
    //     (fun acc v ->
    //         acc
    //         + getPairings pairs v (count - 1) (values - ([ v ] |> Set.ofList)) output)
    //     output

    let allValues = (valueToOperation |> Map.keys |> Set.ofSeq)
    let onePairing = getPairings [] "" 2 (allValues - definitelyOkValues) Set.empty

    // let mutable bestForOne = 0

    // let eligible =
    //     onePairing
    //     |> Set.filter (function
    //         | [] -> failwith "Invalid"
    //         | (a, b) :: _ ->
    //             let aGate = valueToOperation |> Map.find a
    //             let bGate = valueToOperation |> Map.find b

    //             let replaced =
    //                 valueToOperation
    //                 |> Map.add a bGate
    //                 |> Map.add b aGate

    //             match evaluateNumber "z" inputs replaced with
    //             | Some (res, _) ->
    //                 let result = bitArrayToDisplay res

    //                 if seq { 0..maxVal }
    //                    |> Seq.exists (fun idx ->
    //                        result[63 - idx] <> expected[63 - idx]
    //                        && not (mismatched |> Array.contains idx)) then
    //                     false
    //                 else
    //                     let length =
    //                         seq { 0..maxVal }
    //                         |> Seq.filter (fun idx -> result[63 - idx] <> expected[63 - idx])
    //                         |> Seq.length

    //                     if mismatched.Length - length > bestForOne then
    //                         bestForOne <- mismatched.Length - length

    //                     length < mismatched.Length
    //             | None -> false)
    //     |> Set.toSeq
    //     |> Seq.collect (function
    //         | (a, b) :: _ -> [ a; b ]
    //         | _ -> [])
    //     |> Seq.distinct
    //     |> Set.ofSeq

    // let twoPairings = getPairings [] "" 4 (eligible) Set.empty
    // // printfn "%d" twoPairings.Count

    // let eligibleTwo =
    //     twoPairings
    //     |> Set.filter (function
    //         | (a, b) :: (c, d) :: _ ->
    //             let aGate = valueToOperation |> Map.find a
    //             let bGate = valueToOperation |> Map.find b
    //             let cGate = valueToOperation |> Map.find a
    //             let dGate = valueToOperation |> Map.find b

    //             let replaced =
    //                 valueToOperation
    //                 |> Map.add a bGate
    //                 |> Map.add b aGate
    //                 |> Map.add c dGate
    //                 |> Map.add d cGate

    //             match evaluateNumber "z" inputs replaced with
    //             | Some (res, _) ->
    //                 let result = bitArrayToDisplay res

    //                 if seq { 0..maxVal }
    //                    |> Seq.exists (fun idx ->
    //                        result[63 - idx] <> expected[63 - idx]
    //                        && not (mismatched |> Array.contains idx)) then
    //                     false
    //                 else
    //                     let length =
    //                         seq { 0..maxVal }
    //                         |> Seq.filter (fun idx -> result[63 - idx] <> expected[63 - idx])
    //                         |> Seq.length

    //                     // if mismatched.Length - length > bestForOne then
    //                     //     bestForOne <- mismatched.Length - length

    //                     length > bestForOne
    //             | None -> false
    //         | _ -> failwith "Invalid")
    //     |> Set.toSeq
    //     |> Seq.collect (function
    //         | (a, b) :: _ -> [ a; b ]
    //         | _ -> [])
    //     |> Seq.distinct
    //     |> Set.ofSeq

    findPairings [] "" 8 (allValues - definitelyOkValues) mismatched valueToOperation
    |> printfn "%A"
    // printfn "%A" eligibleTwo.Count

    originalZ

let (inputs, operations) = loadData ()

evaluate inputs operations |> printfn "Part 1: %d"
