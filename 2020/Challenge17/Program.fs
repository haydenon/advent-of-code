open System

let parseLine  y (text : string) =
  text.ToCharArray()
  |> Array.mapi (fun x ch -> ((x, y), ch = '#'))
  |> Array.filter snd
  |> Array.map fst

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.mapi parseLine
  |> Array.collect id

let get3DData data =
  data
  |> Array.map (fun (x,y) -> ((x,y,0), ()))
  |> Map.ofArray

let get4DData data =
  data
  |> Array.map (fun (x,y) -> ((x,y,0,0), ()))
  |> Map.ofArray

let getSeq () = seq { -1..1 }

let adjacent3D =
  getSeq()
  |> Seq.allPairs (getSeq())
  |> Seq.allPairs (getSeq())
  |> Seq.map (fun (x, (y, z)) -> (x,y,z))
  |> Seq.filter (fun (x, y, z) -> not(x = 0 && y = 0 && z =0))
  |> Seq.toArray

let adjacent4D =
  getSeq()
  |> Seq.allPairs (getSeq())
  |> Seq.allPairs (getSeq())
  |> Seq.allPairs (getSeq())
  |> Seq.map (fun (x, (y, (z, w))) -> (x,y,z,w))
  |> Seq.filter (fun (x, y, z, w) -> not(x = 0 && y = 0 && z =0 && w = 0))
  |> Seq.toArray

let get3DNeighbours (x, y, z) =
  let getAdjacent (dx, dy, dz) =
    (x + dx, y + dy, z + dz)
  adjacent3D |> Array.map getAdjacent

let get4DNeighbours (x, y, z, w) =
  let getAdjacent (dx, dy, dz, dw) =
    (x + dx, y + dy, z + dz, w + dw)
  adjacent4D |> Array.map getAdjacent

let getThreeDimensions (state : Map<(int * int * int), unit>) =
  let keys =
    state
    |> Map.toArray
    |> Array.map fst
  let getMinMax values =
    let mutable min = Int32.MaxValue
    let mutable max = Int32.MinValue
    for v in values do
      if v < min then min <- v
      if v > max then max <- v
    (min - 1, max + 1)
  let (xmin, xmax) = keys |> Array.map (fun (x,_,_) -> x) |> getMinMax
  let (ymin, ymax) = keys |> Array.map (fun (_,y,_) -> y) |> getMinMax
  let (zmin, zmax) = keys |> Array.map (fun (_,_,z) -> z) |> getMinMax
  (xmin, xmax, ymin, ymax, zmin, zmax)

let getFourDimensions (state : Map<(int * int * int * int), unit>) =
  let keys =
    state
    |> Map.toArray
    |> Array.map fst
  let getMinMax values =
    let mutable min = Int32.MaxValue
    let mutable max = Int32.MinValue
    for v in values do
      if v < min then min <- v
      if v > max then max <- v
    (min - 1, max + 1)
  let (xmin, xmax) = keys |> Array.map (fun (x,_,_,_) -> x) |> getMinMax
  let (ymin, ymax) = keys |> Array.map (fun (_,y,_,_) -> y) |> getMinMax
  let (zmin, zmax) = keys |> Array.map (fun (_,_,z,_) -> z) |> getMinMax
  let (wmin, wmax) = keys |> Array.map (fun (_,_,_,w) -> w) |> getMinMax
  (xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax)

let get3DItemsToCheck state =
  let (xmin, xmax, ymin, ymax, zmin, zmax) = getThreeDimensions state
  seq {
    for x in xmin..xmax do
      for y in ymin..ymax do
        for z in zmin..zmax do
          yield (x,y,z)
  }

let get4DItemsToCheck state =
  let (xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax) = getFourDimensions state
  seq {
    for x in xmin..xmax do
      for y in ymin..ymax do
        for z in zmin..zmax do
          for w in wmin..wmax do
            yield (x,y,z,w)
  }

let getChange getNeighbours state coords =
  let neighbours = getNeighbours coords
  let currState = Map.containsKey coords state
  let active =
    neighbours
    |> Array.map (fun coords -> Map.containsKey coords state)
    |> Array.filter id
    |> Array.length
  if currState && (active < 2 || active > 3) then Some(coords, false)
  else if not currState && active = 3 then Some(coords, true)
  else None

let rec run3D endIter currIter state =
  if currIter >= endIter then state
  else
    let changes =
      get3DItemsToCheck state
      |> Seq.choose (getChange get3DNeighbours state)
    let toRemove = changes |> Seq.filter (fun (_,add) -> not add)
    let toAdd = changes |> Seq.filter (fun (_,add) -> add)
    let upd = toAdd |> Seq.fold (fun st (coords, _) -> Map.add coords () st) state
    let upd = toRemove |> Seq.fold (fun st (coords, _) -> Map.remove coords st) upd
    run3D endIter (currIter + 1) upd

let rec run4D endIter currIter state =
  if currIter >= endIter then state
  else
    let changes =
      get4DItemsToCheck state
      |> Seq.choose (getChange get4DNeighbours state)
    let toRemove = changes |> Seq.filter (fun (_,add) -> not add)
    let toAdd = changes |> Seq.filter (fun (_,add) -> add)
    let upd = toAdd |> Seq.fold (fun st (coords, _) -> Map.add coords () st) state
    let upd = toRemove |> Seq.fold (fun st (coords, _) -> Map.remove coords st) upd
    run4D endIter (currIter + 1) upd

[<EntryPoint>]
let main _ =
  let data = loadData()
  run3D 6 0 (get3DData data)
  |> Map.count
  |> printfn "Active in 3D: %d"

  run4D 6 0 (get4DData data)
  |> Map.count
  |> printfn "Active in 4D: %d"
  0