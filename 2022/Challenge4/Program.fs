open System

let parseSections (sections : string) =
  let parts = sections.Split("-")
  (Int32.Parse parts[0], Int32.Parse parts[1])

let parseRow (row : string) =
  let elfAssignments = row.Split(",")
  (parseSections elfAssignments[0], parseSections elfAssignments[1])

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text
    |> List.ofArray
    |> List.map parseRow

let fullyContained ((amin, amax), (bmin, bmax)) =
  (amin >= bmin && amax <= bmax) ||
  (bmin >= amin && bmax <= amax)

let overlap ((amin, amax), (bmin, bmax)) =
  (amin >= bmin && amin <= bmax) ||
  (bmin >= amin && bmin <= amax) ||
  (amax >= bmin && amin <= bmax) ||
  (bmax >= amin && bmax <= amax)

let data = loadData ()

data
|> List.filter fullyContained
|> List.length
|> printfn "Part 1: %d"


data
|> List.filter overlap
|> List.length
|> printfn "Part 2: %d"
