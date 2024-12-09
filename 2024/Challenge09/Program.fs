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

let moveFiles (array: int array) =
    let length = Array.length array

    let rec getEmptyList list count ind =
        if ind >= length then
            list |> List.rev |> List.toArray
        else if array[ind] <> -1 then
            let newList =
                if count = 0 then
                    list
                else
                    (ind - count, count) :: list

            getEmptyList newList 0 (ind + 1)
        else
            getEmptyList list (count + 1) (ind + 1)

    let list = getEmptyList [] 0 0

    let findNextSpot currIdx count =
        list
        |> Array.tryFindIndex (fun (idx, space) -> space >= count && idx < currIdx)

    let rec move moved num ind count =
        if ind < 0 then
            array
        else if array[ind] <> num then

            let newNum = array[ind]

            // if (moved |> Set.contains num) then
            //   printfn "%d" num
            if num <> -1 && not (moved |> Set.contains num) then
                match findNextSpot (ind + 1) count with
                | Some spotInd ->
                    let (spotStart, spotCount) = list[spotInd]

                    // if (moved |> Set.contains num) then
                    //     printfn "%d %d %d" num spotStart spotCount

                    for i in 0 .. count - 1 do
                        array[spotStart + i] <- num
                        array[ind + 1 + i] <- -1

                    list[spotInd] <- (spotStart + count, spotCount - count)
                    move (moved |> Set.add num) newNum (ind - 1) 1
                | None -> move (moved |> Set.add num) newNum (ind - 1) 1
            else
                move moved newNum (ind - 1) 1
        else
            move moved num (ind - 1) (count + 1)

    move Set.empty (array[array.Length - 1]) (array.Length - 1) 0

let arr = data |> fillArray 0 [] |> List.toArray
let arr1 = arr |> Array.copy
let arr2 = arr |> Array.copy

backfill arr1 0 ((arr1 |> Array.length) - 1)
|> Array.takeWhile (fun num -> num >= 0)
|> Array.map int64
|> Array.indexed
|> Array.sumBy (fun (idx, num) -> (int64 idx) * num)
|> printfn "Part 1: %d"

arr2
|> moveFiles
|> Array.map int64
|> Array.indexed
|> Array.sumBy (fun (idx, num) ->
    if num < 0 then
        0L
    else
        (int64 idx) * num)
|> printfn "%d"
