open System

let getValueOfPos (pos, value) =
  let posValue = 5.0 ** (float pos) |> int64
  match value with
  | '-' -> posValue * -1L
  | '=' -> posValue * -2L
  | '2'-> posValue * 2L
  | '1' -> posValue * 1L
  | '0'-> 0L
  | _ -> failwith "Invalid input"


let parseSnafu (number: string) =
  let parts =
    number.ToCharArray()
    |> Array.rev
    |> Array.indexed
  parts |> Array.map getValueOfPos |> Array.sum

let toBaseFive value : int list =
    let rec loop num digits =
        let q = num / 5L
        let r = num % 5L
        if q = 0 then
            (int r) :: digits
        else
            loop q ((int r) :: digits)

    loop value []

let getSnafu (number : int64) =
  let baseFiveNumbers = toBaseFive number
  let addToNext list value = match list with | [] -> [value] | first :: rest -> (first + value) :: rest
  let rec getSnafuForNumbers values chars =
    match values with
    | [] -> new System.String(chars |> List.toArray )
    | next :: rest ->
      if next >= 5 then
        let toAddToNext = next / 5
        let remainder = next % 5
        getSnafuForNumbers (remainder :: (addToNext rest toAddToNext)) chars
      elif next = 4 then
        getSnafuForNumbers (addToNext rest 1) ('-' :: chars)
      elif next = 3 then
        getSnafuForNumbers (addToNext rest 1) ('=' :: chars)
      else
        getSnafuForNumbers rest ((string next)[0] :: chars)


  getSnafuForNumbers (baseFiveNumbers |> List.rev) []

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> Array.toList

let data = loadData ()

data |> List.map parseSnafu |> List.sum |> getSnafu |> printfn "Part 1: %s"

