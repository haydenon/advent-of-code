open System

let (|TileId|_|) (line : string) =
  if line.StartsWith("Tile ")
  then
    let line = line.Substring(5)
    let mutable i = 0
    while Char.IsDigit line.[i] do i <- i + 1
    Some (line.Substring(0, i) |> int32)
  else None

let (|Tile|_|) (line : string) =
  if line.StartsWith('.') || line.StartsWith('#')
  then Some(line.ToCharArray() |> Array.map ((=) '#') |> Array.toList)
  else None

let rec parseTiles tiles currId currTile lines =
  match lines with
  | TileId id :: tail -> parseTiles tiles id currTile tail
  | Tile row :: tail -> parseTiles tiles currId (row :: currTile) tail
  | _ :: tail -> parseTiles ((currId, List.rev currTile) :: tiles) 0 [] tail
  | [] -> List.rev ((currId, List.rev currTile) :: tiles)

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.toList
  |> parseTiles [] 0 []

let toLineStr  (line : bool[]) =
  line |> Array.map (fun b -> if b then '#' else '.') |> String

let printBorders id ((top, right, bottom, left) : ImageCompose.Borders) : unit =
  printfn " Tile: %d " id
  printfn "%s" (top |> toLineStr)
  let left = (left |> Array.rev |> toLineStr)
  let right = (right |> toLineStr)
  for i in 1..8 do
    printfn "%c        %c" left.[i] right.[i]
  printfn "%s" (bottom |> Array.rev |> toLineStr)
  printfn ""

let getImageStr (image : bool[][]) =
  image
  |> Array.map (toLineStr >> sprintf "%s")
  |> fun lines -> String.Join('\n', lines)

[<EntryPoint>]
let main _ =
  let data = loadData()
  let borders = data |> List.map ImageCompose.mapTileToBorders
  let borderMap = ImageCompose.createBorderMap borders
  let size = Math.Sqrt(float borders.Length) |> int32
  printfn "Cube of size: %d" size
  let solution = ImageCompose.findSquareSolution borderMap borders size |> List.head
  solution
  |> ImageCompose.getCornerMultiplication size
  |> printfn "Corners multiply to: %d"

  let (placements, transforms) = solution
  let image = ImageCompose.stitchImage size data placements transforms
  let (monsterCount, monsterMap) = image |> MonsterSearching.searchImageForMonsters
  let waterRoughness =
    image
    |> Array.indexed
    |> Array.collect (fun (y, line) -> line |> Array.mapi (fun x rough -> ((x, y),rough)) )
    |> Array.filter snd
    |> Array.map fst
  let roughExcludingMonster =
    waterRoughness
    |> Array.filter (monsterMap.ContainsKey >> not)
    |> Array.length
  printfn "Found %d monsters, water roughness is %d" monsterCount roughExcludingMonster
  0