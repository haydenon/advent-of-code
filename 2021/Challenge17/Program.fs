open System

let abs num =
  (Math.Abs : int -> int)(num)

let max n1 n2 =
  (Math.Max : int * int -> int)(n1, n2)

let parseTarget (text : string) =
  let parseAxis (text : string) =
    let coords =
      text.Substring(2).Split("..")
      |> Array.map Int32.Parse
      |> Array.sort
    (coords[0], coords[1])
  let axes =
    text.Split(", ")
    |> Array.map parseAxis
  (axes[0], axes[1])

let loadData () =
  let line =
    System.IO.File.ReadAllLines("./input.txt")
    |> Array.head
  parseTarget (line.Substring(String.length "target area: "))

let rec findFurtherestXTravelled distance velocity =
  match velocity with
  | 0 -> distance
  | _ -> findFurtherestXTravelled (distance + velocity) (velocity - 1)

let findMinMaxXVelocities (minX, maxX) =
  let slowest =
    seq {1..maxX}
    |> Seq.findIndex ((findFurtherestXTravelled 0) >> ((<)minX))
  (slowest + 1, maxX)

let findMinMaxYVelocities (minY, _) =
  (minY, (abs minY) - 1)

type CheckStatus =
  | Inconclusive
  | Lands
  | Fails

let getStatus (xv, yv) (x,y) ((minX, maxX), (minY, maxY)) =
  if x <= maxX && x >= minX && y <= maxY && y >= minY
    then Lands
  else if x <= maxX && (xv > 0 || x >= minX) && y >= minY
    then Inconclusive
  else
    Fails

let rec checkIfVelocityLandsInTarget target loc velocity =
  match getStatus velocity loc target with
  | Lands -> true
  | Fails -> false
  | Inconclusive ->
    let (xv, yv) = velocity
    let (x, y) = loc
    let newVelocity = (max (xv - 1) 0, yv - 1)
    let newLoc = (x + xv, y + yv)
    checkIfVelocityLandsInTarget target newLoc newVelocity

let findAllInitialVelocities target =
  let (xRange, yRange) = target
  let (minX, maxX) = findMinMaxXVelocities xRange
  let (minY, maxY) = findMinMaxYVelocities yRange

  Seq.allPairs (seq {minX..maxX}) (seq {minY..maxY})
  |> Seq.filter (checkIfVelocityLandsInTarget target (0,0))
  |> Set.ofSeq

let data = loadData()

let (_, (lowestPoint, _)) = data
seq { 0..((abs lowestPoint) - 1) }
|> Seq.reduce (+)
|> printfn "Part 1: %d"

findAllInitialVelocities data
|> Set.count
|> printfn "Part 2: %d"