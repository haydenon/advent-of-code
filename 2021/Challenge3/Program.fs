open System
open System.Collections

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map (Seq.toList >> List.toArray)
  |> Array.toList

let data = loadData()

let rec countBits data counts =
  match data with
  | [] -> counts
  | row :: tail ->
    let newCounts =
      Array.zip row counts
      |> Array.map (fun (ch, cnt) -> if ch = '1' then cnt + 1 else cnt)
    countBits tail newCounts


let bitArrayToInt64 (boolArray : bool[]) =
  let bitArray = BitArray(Array.rev boolArray)
  let byteArray: byte[] = Array.zeroCreate 8;
  bitArray.CopyTo(byteArray, 0)
  BitConverter.ToInt64(byteArray, 0);

let getRate data =
  let colCount = data |> List.head |> Array.length
  let rowCount = List.length data
  countBits data (Array.zeroCreate colCount)
  |> Array.map (fun count -> float count / float rowCount)

let rec findValue criteria index (values : char[] list) =
  match values with
  | value :: [] ->
    value |> Array.map (fun ch -> ch = '1')
  | remaining ->
    let bitChecks =
      getRate remaining
      |> Array.map criteria
    remaining
    |> List.filter (fun row -> row[index] = if bitChecks[index] then '1' else '0')
    |> findValue criteria (index + 1)

let rate = getRate data
let gammaRate = rate |> Array.map (fun p -> p >= 0.5)
let epsilonRate = rate |> Array.map (fun p -> p < 0.5)
let part1 = (bitArrayToInt64 gammaRate * bitArrayToInt64 epsilonRate)

printfn "Part 1: %d" part1

let oxygenRating =
  findValue (fun r -> r >= 0.5) 0 data
  |> bitArrayToInt64
let co2Rating =
  findValue (fun r -> r < 0.5) 0 data
  |> bitArrayToInt64
let part2 = oxygenRating * co2Rating
printfn "Part 2: %d" part2
