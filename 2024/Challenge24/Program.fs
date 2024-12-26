open System
open System.Collections.Generic
open System.Collections
open System.Text.RegularExpressions

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
    let inputRegex = Regex(@"^[xy][0-9]{2}$")

    let findPossibleInvalid (valueToOperation: Map<string, string * string * string>) max index =
        let sumName = ("z" + (sprintf "%02d" index))
        let (sumLeft, sumOp, sumRight) = valueToOperation |> Map.find sumName

        let checkCarry carryName =
            let (carryLeft, carryOp, carryRight) = valueToOperation |> Map.find carryName

            if index = 1 then
                let yName = ("y" + (sprintf "%02d" (index - 1)))
                let xName = ("x" + (sprintf "%02d" (index - 1)))

                if carryOp <> "AND"
                   || ([ carryLeft; carryRight ] |> List.sort)
                      <> [ xName; yName ] then
                    [ carryName ]
                else
                    []
            else

            if carryOp <> "OR" then
                [ carryName ]
            else
                let left = valueToOperation |> Map.find carryLeft
                let right = valueToOperation |> Map.find carryRight
                let yName = ("y" + (sprintf "%02d" (index - 1)))
                let xName = ("x" + (sprintf "%02d" (index - 1)))

                let inpAnd =
                    [ left; right ]
                    |> List.filter (fun (leftVal, op, rightVal) ->
                        op = "AND"
                        && ([ leftVal; rightVal ] |> List.sort) = [ xName; yName ])

                if inpAnd |> List.length <> 1 then
                    [ carryLeft; carryRight ]
                else
                    let inputAndWasLeft = left = (inpAnd |> List.head)

                    let checkCarrySumAndPrevCarry name =
                        let (carrySumPrevLeft, carrySumPrevOp, carrySumPrevRight) =
                            valueToOperation |> Map.find name

                        if
                            carrySumPrevOp <> "AND"
                            || inputRegex.IsMatch(carrySumPrevLeft)
                            || inputRegex.IsMatch(carrySumPrevRight)
                        then
                            [ name ]
                        else
                            let (leftLeft, leftOp, leftRight) = valueToOperation |> Map.find carrySumPrevLeft

                            let (rightLeft, rightOp, rightRight) =
                                valueToOperation |> Map.find carrySumPrevRight

                            let gates =
                                if index = 2 then
                                    [ "AND"; "XOR" ]
                                else
                                    [ "OR"; "XOR" ]

                            if [ leftOp; rightOp ] |> List.sort <> gates then
                                [ carrySumPrevLeft; carrySumPrevRight ]
                            else
                                let xorIsLeft = leftOp = "XOR"

                                let (xorLeft, xorRight) =
                                    if xorIsLeft then
                                        (leftLeft, leftRight)
                                    else
                                        (rightLeft, rightRight)

                                let invalid =
                                    if ([ xorLeft; xorRight ] |> Set.ofList)
                                       <> ([ yName; xName ] |> Set.ofList) then
                                        let invalidName =
                                            if xorIsLeft then
                                                carrySumPrevLeft
                                            else
                                                carrySumPrevRight

                                        [ invalidName ]
                                    else
                                        []

                                invalid

                    checkCarrySumAndPrevCarry (
                        if inputAndWasLeft then
                            carryRight
                        else
                            carryLeft
                    )

        if index = max then
            checkCarry sumName
        else if sumOp <> "XOR" then
            [ sumName ]
        else if index = 0 then
            if
                not (inputRegex.IsMatch sumLeft)
                || not (inputRegex.IsMatch sumRight)
            then
                [ sumName ]
            else
                []

        else
            match valueToOperation |> Map.tryFind sumLeft, valueToOperation |> Map.tryFind sumRight with
            | Some (leftLeft, leftOp, leftRight), Some (rightLeft, rightOp, rightRight) ->
                let yName = ("y" + (sprintf "%02d" index))
                let xName = ("x" + (sprintf "%02d" index))

                let gates =
                    if index = 1 then
                        [ "AND"; "XOR" ]
                    else
                        [ "OR"; "XOR" ]

                if gates <> ([ leftOp; rightOp ] |> List.sort) then
                    [ sumLeft; sumRight ]
                else
                    let leftInput = leftOp = "XOR"

                    let (inputLeft, inputRight) =
                        if leftInput then
                            (leftLeft, leftRight)
                        else
                            (rightLeft, rightRight)

                    let invalid =
                        if (inputLeft <> yName && inputRight <> yName)
                           || (inputLeft <> xName && inputRight <> xName) then
                            [ if leftInput then sumLeft else sumRight ]
                        else
                            []

                    checkCarry (if leftInput then sumRight else sumLeft)
                    @ invalid
            | _ -> [ sumRight; sumLeft ]


    let invalidPairings = HashSet<(string * string) list>()

    let rec findPairings
        pairs
        previous
        count
        (values: Set<string>)
        (valueToOperation: Map<string, string * string * string>)
        : Option<(string * string) list> =
        if count = 0 then
            let updatedOps =
                pairs
                |> List.fold
                    (fun acc (a, b) ->
                        let aGate = acc |> Map.find a
                        let bGate = acc |> Map.find b

                        acc |> Map.add b aGate |> Map.add a bGate)
                    valueToOperation

            let noInvalid =
                seq { 0..maxVal }
                |> Seq.forall (fun idx ->
                    findPossibleInvalid updatedOps maxVal idx
                    |> List.isEmpty)

            if noInvalid then
                Some(pairs)
            else
                invalidPairings.Add(sortList (pairs |> List.map sort))
                |> ignore

                None
        else if count % 2 = 1 then
            values
            |> Seq.tryPick (fun v ->
                let sorted = (sort (previous, v))

                if invalidPairings.Contains(sortList (sorted :: pairs)) then
                    None
                else
                    let result =
                        findPairings (sorted :: pairs) "" (count - 1) (values - ([ v ] |> Set.ofList)) valueToOperation

                    result)
        else
            values
            |> Seq.tryPick (fun v -> findPairings pairs v (count - 1) (values - ([ v ] |> Set.ofList)) valueToOperation)

    let possiblyInvalid =
        seq { 0..maxVal }
        |> Seq.collect (fun idx -> findPossibleInvalid valueToOperation maxVal idx)
        |> Set.ofSeq

    let result =
        findPairings [] "" 8 possiblyInvalid valueToOperation
        |> Option.get

    let allValues =
        result
        |> List.collect (fun (a, b) -> [ a; b ])
        |> List.sort

    let pairs = String.Join(",", allValues)

    originalZ, pairs

let (inputs, operations) = loadData ()

let part1, part2 = evaluate inputs operations

printfn "Part 1: %d" part1
printfn "Part 2: %s" part2
