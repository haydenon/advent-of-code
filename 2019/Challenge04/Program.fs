open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    let parts = text[ 0 ].Split("-") |> Array.map Int32.Parse
    (parts[0], parts[1])

let (bottom, top) = loadData ()

let matchesPasswordRule1 num =
    let str = num.ToString()

    let hasAdjacent (str: string) =
        seq { 0 .. (str.Length - 2) }
        |> Seq.exists (fun idx -> str[idx] = str[idx + 1])

    let doesNotDecrease (str: string) =
        seq { 0 .. (str.Length - 2) }
        |> Seq.forall (fun idx -> str[idx] <= str[idx + 1])

    hasAdjacent str && doesNotDecrease str

let matchesPasswordRule2 num =
    let str = num.ToString()
    let length = str.Length

    let hasAdjacent (str: string) =
        seq { 0 .. (length - 2) }
        |> Seq.exists (fun idx ->
            str[idx] = str[idx + 1]
            && (idx = length - 2 || str[idx + 2] <> str[idx])
            && (idx = 0 || str[idx - 1] <> str[idx]))

    let doesNotDecrease (str: string) =
        seq { 0 .. (str.Length - 2) }
        |> Seq.forall (fun idx -> str[idx] <= str[idx + 1])

    hasAdjacent str && doesNotDecrease str

seq { bottom..top }
|> Seq.filter matchesPasswordRule1
|> Seq.length
|> printfn "Part 1: %d"

seq { bottom..top }
|> Seq.filter matchesPasswordRule2
|> Seq.length
|> printfn "Part 2: %d"