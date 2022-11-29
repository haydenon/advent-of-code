open System
open System.Collections
open System.Linq

let parseBool = function '0' -> false | _ -> true

type Op =
  | SetMask of (bool * int)[]
  | SetMem  of (int * int64)

let parseMask (text : string) =
  text.Split('=').[1].Trim().ToCharArray()
  |> Array.rev
  |> Array.indexed
  |> Array.choose (function (_,'X') -> None | (ind, ch) -> Some(parseBool ch, ind))
  |> SetMask

let parseMemSet (text : string) =
  let close = text.IndexOf(']')
  let addr  = Int32.Parse (text.Substring(4, close - 4))
  let value = text.Split('=').[1].Trim() |> Int64.Parse
  SetMem(addr, value)

let parseLine (line : string) =
  if line.StartsWith("mask")
  then parseMask line
  else parseMemSet line

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseLine
  |> Array.toList

let int64ToBitArray (num : int64) =
  let numArr: byte array = BitConverter.GetBytes num
  BitArray(numArr)

let bitArrayToInt64 (bitArray : BitArray) =
  let byteArray: byte[] = Array.zeroCreate 8;
  bitArray.CopyTo(byteArray, 0)
  BitConverter.ToInt64(byteArray, 0);

let bitArrToArray (bitArray : BitArray) =
  (bitArray :> IEnumerable).Cast<Boolean>()
  |> Seq.toArray

let updMap value map key = Map.change key (fun _ -> Some value) map

let rec runSets memSets mask addresses =
  match memSets with
  | SetMem (addr, num) :: tail ->
    let bitArray = int64ToBitArray num
    for (value, ind) in mask do
      bitArray.Set(ind, value)
    let result = bitArrayToInt64 bitArray
    let upd = updMap result addresses addr
    runSets tail mask upd
  | SetMask(newMask):: tail ->
    runSets tail newMask addresses
  | [] -> addresses

type Mask = { FloatingIndices : int list
              SetValues       : (bool * int)[] }

let emptyMask = { FloatingIndices = []
                  SetValues = [||] }

let getFloatingIndices (mask : (bool * int)[]) =
  let isFloating i = Array.forall (fun (_,mi) -> mi <> i) mask
  seq { 0..35}
  |> Seq.filter isFloating
  |> Seq.toList

let setArr boolArray toSet =
  let copy = boolArray |> Array.copy
  for (bit, ind) in toSet do
    copy.[ind] <- bit
  copy

let allCombinations lst =
  let rec comb accLst elemLst =
      match elemLst with
      | h::t ->
          let next = [h]::List.map (fun el -> h::el) accLst @ accLst
          comb next t
      | _ -> accLst
  ([] :: comb [] lst)

let getAddressesToSet initialAddr { FloatingIndices = floating; SetValues = setValues } =
  let bitArray = int64ToBitArray initialAddr
  let setValues = setValues |> Array.filter fst
  for (value, ind) in setValues do
      bitArray.Set(ind, value)
  let boolArray = bitArrToArray bitArray
  let getFloatingSet contained =
    floating
    |> List.map (fun i -> (List.contains i contained, i))
  allCombinations floating
  |> Seq.map (getFloatingSet >> (setArr boolArray) >> BitArray >> bitArrayToInt64)

let rec runSetsPartTwo memSets mask addresses =
  match memSets with
  | SetMem (addr, num) :: tail ->
    let addressesToSet = getAddressesToSet (int64 addr) mask
    let upd =
      addressesToSet
      |> Seq.fold (updMap num) addresses
    runSetsPartTwo tail mask upd
  | SetMask(newMaskValues):: tail ->
    let newMask = { FloatingIndices = getFloatingIndices newMaskValues
                    SetValues = newMaskValues }
    runSetsPartTwo tail newMask addresses
  | [] -> addresses

[<EntryPoint>]
let main _ =
  let ops = loadData()
  runSets  ops [||] Map.empty
  |> Map.toArray
  |> Array.sumBy snd
  |> printfn "First sum:  %d"

  runSetsPartTwo ops emptyMask Map.empty
  |> Map.toArray
  |> Array.sumBy snd
  |> printfn "Second sum: %d"
  0