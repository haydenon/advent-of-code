open System
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
  let step3 = step2 * 2048L |>  mixAndPrune step2
  step3

let getNResult num secret =
  seq { 1..num}
  |> Seq.fold (fun acc _ -> getNext acc) secret

let data = loadData()

data
|> Array.map (getNResult 2000)
|> Array.sum
|> printfn "Part 1: %d"