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
|> printfn "%A"