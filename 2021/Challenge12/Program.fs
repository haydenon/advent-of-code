open System
let rec parseInput (lines : string list) nodeMap   =
  let addDestinationToNode node destination nodeMap  =
    nodeMap
    |> Map.change node
      (function
        | None -> Some [destination]
        | Some existing -> Some (destination :: existing))
  match lines with
  | line :: tail ->
    let nodes = line.Split("-")
    let (a,b) = (nodes[0],nodes[1])
    nodeMap
    |> addDestinationToNode a b
    |> addDestinationToNode b a
    |> parseInput tail
  | [] -> nodeMap

let loadData () =
  let lines = System.IO.File.ReadAllLines("./input.txt") |> Array.toList
  parseInput lines Map.empty

let addVisit visits node =
  if Map.containsKey node visits
  then Map.add node (visits[node] + 1) visits
  else Map.add node 1 visits

let rec findPaths canVisit (nodeMap : Map<string, string list>) visits path node =
  if node = "end"
  then ["end" :: path |> List.rev]
  else
    let adjacent =
      nodeMap[node]
      |> List.filter (canVisit visits)
      |> Seq.toList
    match adjacent with
    | [] -> []
    | ajc ->
      let newVisits = addVisit visits node
      let newPath = node :: path
      ajc |> List.collect (findPaths canVisit nodeMap newVisits newPath)

let data = loadData()

let isUppercase (node : string) =
  node.ToCharArray() |> Seq.forall (Char.IsUpper)

let canVisitSmallOnce visits node =
  if node = "start"
    then false
  else if isUppercase node
    then true
  else
    let visitCount =
      visits
      |> Map.tryFind node
      |> Option.defaultValue 0
    visitCount < 1

let canVisitOneSmallTwice twiceNode visits node =
  if node = "start"
    then false
  else if isUppercase node
    then true
  else
    let allowedCount = if node = twiceNode then 2 else 1
    let visitCount =
      visits
      |> Map.tryFind node
      |> Option.defaultValue 0
    visitCount < allowedCount

let originalCount =
  findPaths canVisitSmallOnce data Map.empty [] "start"
  |> List.length

printfn "Part 1: %d" originalCount

data
|> Map.keys
|> Seq.toList
|> List.filter (fun node -> node <> "start" && node <> "end" && (not (isUppercase node)))
|> List.map (fun node -> findPaths (canVisitOneSmallTwice node) data Map.empty [] "start")
|> List.map (fun paths -> List.length paths - originalCount)
|> List.sum
|> (fun sum -> sum + originalCount)
|> printfn "Part 2: %d"