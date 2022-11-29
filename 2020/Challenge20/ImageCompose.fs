module ImageCompose

type Borders = bool[] * bool[] * bool[] * bool[]

type TileBorders = { Id     : int
                     Orig   : Borders
                     FlipX  : Borders
                     FlipY  : Borders
                     FlipXY : Borders }

let private getBorders rows =
  let top = rows |> List.head
  let right = rows |> List.map List.last
  let bottom = rows |> List.last |> List.rev
  let left = rows |> List.map List.head |> List.rev
  (top, right, bottom, left)

let mapTileToBorders (id, rows) =
  let m = List.toArray
  let r = List.rev
  let (top, right, bottom, left) = getBorders rows
  let (fxytop, fxyright, fxybottom, fxyleft) = (r bottom, r left, r top, r right)
  let (fxtop, fxright, fxbottom, fxleft) = (r top, r left, r bottom, r right)
  let (fytop, fyright, fybottom, fyleft) = (r bottom, r right, r top, r left)
  let orig = (m top, m right, m bottom, m left)
  let flipxy = (m fxytop, m fxyright, m fxybottom, m fxyleft)
  let flipx = (m fxtop, m fxright, m fxbottom, m fxleft)
  let flipy = (m fytop, m fyright, m fybottom, m fyleft)
  { Id = id; Orig = orig; FlipXY = flipxy; FlipX = flipx; FlipY = flipy }

let private getAllBorders { Orig = (t,r,b,l); FlipXY = (ft,fr,fb,fl)} = [t;r;b;l;ft;fr;fb;fl]

let private getBorderList ((t,r,b,l) : Borders) = [t;r;b;l]

let private getBorderById  borders id =
  borders |> List.find (fun { Id = bid } -> bid = id)

let private getBorder ind { Orig = orig; FlipX = flipx; FlipY = flipy } (xflip, yflip, rotation) =
  let borderList =
    if xflip then flipx
    else if yflip then flipy
    else orig
  let values = getBorderList borderList
  let ind = (ind + rotation) % 4
  values.[ind]

type TransformMap = Map<int, bool * bool * int>

let private getAdjBorder borders placements (transforms : TransformMap) coord border =
  let adj = coord
  if Map.containsKey adj placements
  then
    let adjId = placements.[adj]
    let tile = getBorderById borders adjId
    getBorder border tile transforms.[adjId] |> Array.rev |> Some
  else None

let private findTransforms tile aboveBorder leftBorder =
  let origBorders = getBorderList tile.Orig |> List.toArray
  let flipxBorders = getBorderList tile.FlipX |> List.toArray
  let flipyBorders = getBorderList tile.FlipY |> List.toArray
  let options = [false,false, origBorders; true, false, flipxBorders; false, true, flipyBorders]
  let getOne border start =
    let getOneFromBorders (flipx, flipy, (borders : bool[][])) =
      seq { 0..3 }
      |> Seq.tryFindIndex (fun ind -> borders.[(ind + start) % 4] = border)
      |> Option.map (fun ind -> (flipx, flipy, ind))
    options
    |> List.choose getOneFromBorders
  let getTwo left above  =
    let getTwoFromBorders (flipx, flipy, (borders : bool[][])) =
      seq { 0..3 }
      |> Seq.tryFindIndex (fun ind -> borders.[(ind + 3) % 4] = left && borders.[ind % 4] = above)
      |> Option.map (fun ind -> (flipx, flipy, ind))

    options
    |> List.choose getTwoFromBorders

  match (leftBorder, aboveBorder) with
  | None, None -> failwith ""
  | Some left, None -> getOne left 3
  | None, Some above -> getOne above 0
  | Some left, Some above -> getTwo left above

let findSquareSolution (borderMap : Map<bool[], int list>) borders size =
  let coords = seq {
      for y in 0..size - 1 do
        for x in 0..size - 1 do
          yield(x,y) } |> List.ofSeq |> List.skip 1
  let getRemaining above left placements =
    let adjb = [above; left] |> List.choose id
    let taken = placements |> Map.toList |> List.map snd
    let remaining = borders |> List.map (fun { Id = id } -> id) |> List.filter (fun id -> not(List.contains id taken))
    let adj = adjb |> List.map (fun b -> borderMap.[b])
    remaining |> List.filter (fun id -> List.forall (fun als -> List.contains id als) adj)
  let getAdjBorder = getAdjBorder borders
  let rec findSquare placements transforms coords =
    match coords with
    | [] -> [(placements, transforms)]
    | (x,y) :: next ->
      let aboveBorder = getAdjBorder placements transforms (x, y - 1) 2
      let leftBorder = getAdjBorder placements transforms (x - 1, y) 1
      let applySquare id =
        let tile = getBorderById borders id
        let updp = Map.change (x,y) (fun _ -> Some id) placements
        let applyTrans transform = Map.change id (fun _ -> Some transform) transforms
        findTransforms tile aboveBorder leftBorder
        |> List.toSeq
        |> Seq.collect (fun transform -> findSquare updp (applyTrans transform) next)
        |> Seq.truncate 1
        |> List.ofSeq
      let remaining = getRemaining aboveBorder leftBorder placements
      remaining
      |> List.collect applySquare
      |> List.truncate 1
  let options =
    [0;1;2;3]
    |> List.allPairs [(false, false); (true, false); (false,true)]//(List.allPairs [true; false] [true; false])
    |> List.map (fun ((x, y), r) -> (x,y,r))
  let startWith ({Id = id }, transform) =
    let placements = Map.ofList [((0,0), id)]
    let transforms = Map.ofList [(id, transform)]
    findSquare placements transforms coords

  let corners =
    borders
    |> List.filter (fun b -> getBorderList b.Orig |> List.filter (fun bord -> borderMap.[bord].Length = 1) |> List.length > 1)

  options
  |> List.allPairs corners
  |> List.collect startWith
  |> List.truncate 1

let getCornerMultiplication size (placements : Map<int *int, int>, _)  =
  let max = size - 1
  let corners = List.allPairs [0; max] [max; 0]
  corners
  |> List.fold (fun acc coord -> int64 placements.[coord] * acc) 1L

let createBorderMap borders : Map<bool[], int list> =
  let getForTile { Id = id; Orig = orig; FlipX = flipx; FlipY = flipy; FlipXY = flipxy } =
    let borderList = getBorderList orig @ getBorderList flipx @ getBorderList flipy
    borderList |> List.map (fun bl -> (bl, id))
  borders
  |> List.collect getForTile
  |> List.groupBy fst
  |> List.map (fun (b, values) -> (b, List.map snd values))
  |> Map.ofList

let private writeSection (image : bool[][]) (tile : bool[][]) size startx starty transform =
  let (flipx, flipy, rotation) = transform
  let getCoord x y =
    let (x,y) =
      match rotation with
      | 0 -> (x, y)
      | 1 -> (size + 1 - y , x)
      | 2 -> (size + 1 - x, size + 1 - y)
      | 3 -> (y, size + 1 - x)
      | _ -> failwith "Invalid rotation"
    let x = if flipx then (size + 1) - x else x
    let y = if flipy then (size + 1) - y else y
    (x, y)
  for y in 1..size do
    for x in 1..size do
      let (tilex, tiley) = getCoord x y
      image.[starty + y - 1].[startx + x - 1] <- tile.[tiley].[tilex]

let stitchImage
  size
  (tiles : (int32 * bool list list) list)
  (placements : Map<int * int, int>)
  (transforms : TransformMap) : bool[][] =
    let borderlessSize = 8
    let totalSize = borderlessSize * size
    let image = Array.init totalSize (fun _ -> Array.zeroCreate totalSize)
    for y in 0..size - 1 do
      for x in 0..size - 1 do
        let tileId = placements.[x, y]
        let tile =
          tiles
          |> List.find (fun (id,_) -> id = tileId)
          |> snd
          |> List.map(List.toArray)
          |> List.toArray
        let transform = transforms.[tileId]
        let sx = x * borderlessSize
        let sy = y * borderlessSize
        writeSection image tile borderlessSize sx sy transform
    image