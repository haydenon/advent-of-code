open System

type Index = uint
type Number =
  | Complex of Number * Number
  | Literal of Index

type NumberWithValues = Number * int[]

let (|Open|_|) (str: string) =
  if str.Length > 0 && str[0] = '['
  then Some(Open)
  else None

let (|Number|_|) (str : string) =
  let rec parseNumberValue numStr (str : string) =
    if Char.IsDigit str[0] |> not
    then Int32.Parse numStr
    else parseNumberValue (numStr + (str[0].ToString())) (str.Substring(1))
  if Char.IsDigit str[0]
  then parseNumberValue "" str |> Some
  else None

let parseNumber text =
  let rec parse numbers index (text : string) =
    match text with
    | Open ->
      let (first, firstLength, firstNumbers, firstIndex) = parse numbers index (text.Substring(1))
      let (second, secondLength, secondNumbers, secondIndex) = parse firstNumbers firstIndex (text.Substring(2 + firstLength))
      (Complex(first, second), 3 + firstLength + secondLength, secondNumbers, secondIndex)
    | Number l ->
      let newNumbers = l :: numbers
      (Literal index, l.ToString().Length, newNumbers, (index + 1u))
    | _ -> failwith "Invalid number"
  let (numberTree, _, numbers, _) = parse [] 0u text
  (numberTree, numbers |> List.rev |> List.toArray)

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseNumber
  |> Array.toList

let printNumber ((number, values) : NumberWithValues) =
  let rec getStringForNumber number =
    match number with
    | Literal ind -> values[int ind].ToString()
    | Complex(left, right) -> sprintf "[%s,%s]" (getStringForNumber left) (getStringForNumber right)
  getStringForNumber number
  |> printfn "%s"

let rec updateTree tryReplaceNode updateIndex number =
  match tryReplaceNode number with
  | Some newNumber -> newNumber
  | None ->
    match number with
    | Literal ind -> Literal(updateIndex ind)
    | Complex(left, right) ->
      let newLeft = updateTree tryReplaceNode updateIndex left
      let newRight = updateTree tryReplaceNode updateIndex right
      Complex(newLeft, newRight)

let updateTreeWithExplode ind =
  let replaceNode number =
    match number with
    | Complex(Literal left, _) when left = ind -> Some(Literal ind)
    | _ -> None
  let updateIndex index = if index > ind then (index - 1u) else index
  updateTree replaceNode updateIndex

let rec tryFindExplodeIndex depth number =
  match number with
  | Literal _ -> None
  | Complex (Literal left, Literal _) when depth = 4 -> Some(left)
  | Complex (left, right) ->
    match tryFindExplodeIndex (depth + 1) left with
    | Some index -> Some index
    | None -> tryFindExplodeIndex (depth + 1) right

let tryExplodeNumber ((number, values) : NumberWithValues) =
  let updateNumbers ind (values : int[]) =
    if ind > 0 then
      values[ind - 1] <- values[ind - 1] + values[ind]
    if ind < (Array.length values) - 2 then
      values[ind + 2] <- values[ind + 2] + values[ind + 1]
    values[ind] <- 0
    Array.removeAt (ind + 1) values
  match tryFindExplodeIndex 0 number with
  | Some ind ->
    Some(updateTreeWithExplode ind number, updateNumbers (int ind) values)
  | None -> None

let updateTreeWithSplit ind =
  let replaceNode number =
    match number with
    | Literal index when index = ind -> Some(Complex(Literal ind, Literal(ind + 1u)))
    | _ -> None
  let updateIndex index = if index > ind then (index + 1u) else index
  updateTree replaceNode updateIndex

let rec tryFindSplitIndex (values : int[]) number =
  match number with
  | Literal ind -> if values[int ind] >= 10 then Some ind else None
  | Complex (left, right) ->
    match tryFindSplitIndex values left with
    | Some index -> Some index
    | None -> tryFindSplitIndex values right

let trySplitNumber ((number, values) : NumberWithValues) =
  let updateNumbers ind (values : int[]) =
    let existingValue = values[int ind]
    let leftValue = existingValue / 2
    let rightValue = existingValue - leftValue
    values[int ind] <- leftValue
    Array.insertAt (ind + 1) rightValue values
  match tryFindSplitIndex values number with
  | Some ind ->
    Some(updateTreeWithSplit ind number, updateNumbers (int ind) values)
  | None -> None

let rec reduceNumber numberValues =
  match tryExplodeNumber numberValues with
  | Some numberValues -> reduceNumber numberValues
  | None ->
    match trySplitNumber numberValues with
    | Some numberValues -> reduceNumber numberValues
    | None -> numberValues

let addNumbers ((firstNumber, firstValues) : NumberWithValues) ((secondNumber, secondValues) : NumberWithValues) =
  let firstCount = Array.length firstValues
  let dontReplace _ = None
  let increment ind = ind + uint firstCount
  let secondTree = updateTree dontReplace increment secondNumber
  let newNumberValues = Complex(firstNumber, secondTree), Array.append firstValues secondValues
  reduceNumber newNumberValues

let getMagnitude ((number, values) : NumberWithValues) =
  let rec getValue number =
    match number with
    | Literal ind -> values[int ind]
    | Complex(left, right) ->
      getValue left * 3 + getValue right * 2
  getValue number

let max n1 n2 =
  (Math.Max : int * int -> int)(n1, n2)

let getGreatest ((firstNumber, secondNumber) : NumberWithValues * NumberWithValues) =
  max (addNumbers firstNumber secondNumber |> getMagnitude) (addNumbers secondNumber  firstNumber |> getMagnitude)

let data = loadData()

data
|> List.reduce addNumbers
|> getMagnitude
|> printfn "Part 1: %d"

List.allPairs data data
|> List.maxBy getGreatest
|> getGreatest
|> printfn "Part 2: %d"