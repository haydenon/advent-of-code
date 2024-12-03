open System
open System.Text.RegularExpressions

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    String.Join("", text)

let data = loadData ()

let regex = Regex(@"mul\(([0-9]{1,3}),([0-9]{1,3})\)", RegexOptions.Compiled)
let doRegex = Regex(@"do\(\)", RegexOptions.Compiled)
let dontRegex = Regex(@"don't\(\)", RegexOptions.Compiled)

let parseMatch (m: Match) =
    (m.Groups[1].Value |> Int64.Parse, m.Groups[2].Value |> Int64.Parse)

let addMul acc (a, b) = acc + a * b

regex.Matches(data)
|> Seq.map parseMatch
|> Seq.fold addMul 0L
|> printfn "Part 1: %d"

let parts = dontRegex.Split(data)

let enabledParts =
    (parts[0]
     :: (parts
         |> Array.skip 1
         |> Array.map (
             (fun str -> doRegex.Split(str))
             >> Array.skip 1
             >> (fun strs -> String.Join("", strs))
         )
         |> Array.toList))

regex.Matches(String.Join("", enabledParts))
|> Seq.map parseMatch
|> Seq.fold addMul 0L
|> printfn "Part 2: %d"

