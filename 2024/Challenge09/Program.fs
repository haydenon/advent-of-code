open System

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text[0]
    |> Seq.toArray
    |> Array.map (string >> int32)
    |> Array.toList

let data = loadData ()

let rec fillArray ind res list =
    match list with
    | count :: space :: rest ->
        let newRes =
            List.append
                (List.append
                    res
                    (seq { 0 .. (count - 1) }
                     |> Seq.map (fun _ -> ind)
                     |> Seq.toList))
                (seq { 0 .. (space - 1) }
                 |> Seq.map (fun _ -> -1)
                 |> Seq.toList)

        fillArray (ind + 1) newRes rest
    | [] -> res
    | count :: rest ->
        let newRes =
            List.append
                res
                (seq { 0 .. (count - 1) }
                 |> Seq.map (fun _ -> ind)
                 |> Seq.toList)

        fillArray (ind + 1) newRes rest

let rec backfill (array: int array) ind back =
    match array[ind] with
    | -2 -> array
    | -1 ->
        if array[back] = -1 then
            backfill array ind (back - 1)
        else
            array[ind] <- array[back]
            array[back] <- -2
            backfill array (ind + 1) (back - 1)
    | _ -> backfill array (ind + 1) back

let arr = data |> fillArray 0 [] |> List.toArray

backfill arr 0 ((arr |> Array.length) - 1)
|> Array.takeWhile (fun num -> num >= 0)
|> Array.map int64
|> Array.indexed
|> Array.sumBy (fun (idx, num) -> (int64 idx) * num)
|> printfn "%A"
