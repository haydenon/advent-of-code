open System
open System.Collections.Generic

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text
    |> Array.map (fun line ->
        let parts = line.Split("-")
        (parts[0], parts[1]))
    |> Array.toList

let data = loadData ()

type Node =
    { Name: string
      Connections: Set<string> }

let buildGraph connections =
    let addConnection source dest graph =
        graph
        |> Map.change source (function
            | Some node -> Some({ node with Connections = node.Connections |> Set.add dest })
            | None ->
                Some(
                    { Name = source
                      Connections = Set.empty |> Set.add dest }
                ))

    connections
    |> List.fold (fun acc (c1, c2) -> addConnection c1 c2 acc |> addConnection c2 c1) Map.empty

let graph = data |> buildGraph

let getKey k1 k2 k3 =
    String.Join("-", [ k1; k2; k3 ] |> List.sort)

let rec getGroups (graph: Map<string, Node>) nodes (output: Set<string>) =
    match nodes with
    | [] -> output
    | nodeName :: rest ->
        let node = graph |> Map.find nodeName

        let newOutput =
            node.Connections
            |> Seq.fold
                (fun acc c ->
                    let other = graph |> Map.find c

                    let inCommon =
                        other.Connections
                        |> Set.intersect node.Connections
                        |> Seq.map (fun third -> getKey nodeName c third)
                        |> Set.ofSeq

                    acc + inCommon)
                output

        getGroups graph rest newOutput

let tNodes =
    graph
    |> Map.keys
    |> Seq.filter (fun k -> k.StartsWith("t"))
    |> Seq.toList

getGroups graph tNodes Set.empty
|> Set.count
|> printfn "Part 1: %d"

let visited = HashSet<string>()

let rec findLargestGroup (graph: Map<string, Node>) nodes (output: string list list) =
    let rec getGroup (nodeName) =
        let node = graph |> Map.find nodeName
        visited.Add nodeName |> ignore

        let rec findCommon acc visited =
            match acc - visited |> Set.toSeq |> Seq.tryHead with
            | Some next ->
                let otherConnections =
                    ((graph |> Map.find next).Connections
                     + ([ next ] |> Set.ofList))

                let intersection = otherConnections |> Set.intersect acc
                findCommon intersection (visited |> Set.add next)
            | None -> acc

        node.Connections
        |> Set.toList
        |> List.map (fun c ->
            let otherConnections = ((graph |> Map.find c).Connections.Add(c))

            let intersection =
                otherConnections
                |> Set.intersect (node.Connections.Add nodeName)

            findCommon intersection ([ nodeName; c ] |> Set.ofList))
        |> List.maxBy Set.count

    match nodes with
    | [] -> output
    | nodeName :: rest ->
        if visited.Contains nodeName then
            findLargestGroup graph rest output
        else
            let grouping = getGroup nodeName |> Set.toList
            findLargestGroup graph rest (grouping :: output)

findLargestGroup graph (graph |> Map.keys |> Seq.toList) []
|> List.maxBy List.length
|> List.sort
|> fun chars -> String.Join(",", chars)
|> printfn "%s"
