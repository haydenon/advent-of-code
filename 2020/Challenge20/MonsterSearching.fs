module MonsterSearching

open System.Collections.Generic

let private monsterPattern = [|(18,0);(0,1);(5,1);(6,1);(11,1);(12,1);(17,1);(18,1);(19,1);(1,2);(4,2);(7,2);(10,2);(13,2);(16,2)|]

let private options =
  [|0;1;2;3|]
  |> Array.allPairs [|(false, false); (true, false); (false,true)|]
  |> Array.map (fun ((x, y), r) -> (x,y,r))

let private rotateValue rotation (x,y) =
  match rotation with
  | 0 -> (x, y)
  | 1 -> (y * -1 , x)
  | 2 -> (x * -1, y * -1)
  | 3 -> (y, x * -1)
  | _ -> failwith "Invalid rotation"

let private flipHorizontal width (x,y) =
  (width - 1 - x, y)

let private flipVertical height (x,y) =
  (x, height - 1 - y)

let private startAtOrigin values =
  let xs = values |> Array.map fst
  let ys = values |> Array.map snd
  let xOffset = xs |> Array.min
  let yOffset = ys |> Array.min
  values |> Array.map (fun (x,y) -> (x - xOffset, y - yOffset))

let private allPatterns =
  let applyTransform (flipx, flipy, rotate) =
    let applyToPoint coord =
      let coord = rotateValue rotate coord
      if flipx then flipHorizontal 20 coord
      else if flipy then flipVertical 3 coord
      else coord
    monsterPattern |> Array.map applyToPoint |> startAtOrigin

  options
  |> Array.map applyTransform


let private getMonsterCoords (x,y) (mx, my) =
  (mx + x, my + y)

let private isMonsterAtPoint (image : bool[][]) pattern coords =
  let size = image.Length
  let checkCoordinate mcoords =
    let (x, y) = getMonsterCoords coords mcoords
    if x >= 0 && x < size && y >= 0 && y < size
    then image.[y].[x]
    else false
  pattern
  |> Array.forall checkCoordinate

let searchImageForMonsters (image : bool[][]) =
  let monsterMap = Dictionary<int * int, unit>()
  let size = image.Length
  let mutable monsters = 0
  for pattern in allPatterns do
    for y in 0..size - 1 do
      for x in 0..size - 1 do
        if isMonsterAtPoint image pattern (x, y) then
          let monsterCoords = pattern |> Array.map (getMonsterCoords (x,y))
          for mcoords in monsterCoords do
            monsterMap.[mcoords] <- ()
          monsters <- monsters + 1
  (monsters, monsterMap)