open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Node = Node of string * int * string list

module Node =
    let getName (Node (name, _, _)) = name

    let getFlow (Node (_, flow, _)) = flow

let rx =
    Regex(@"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)", RegexOptions.Compiled)

let parseValue (line: string) =
    let m = rx.Match(line)
    let edges = m.Groups[ 3 ].Value.Split(", ") |> List.ofArray
    Node(m.Groups[1].Value, m.Groups[2].Value |> Int32.Parse, edges)

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.toList |> List.map parseValue

let getForName nodes nodeName =
    nodes
    |> List.find (function
        | Node (name, _, _) -> name = nodeName)

let findShortestPath nodes (start, target) =
    let startNode = getForName nodes start

    let queue = PriorityQueue<Node, int>()
    queue.Enqueue(startNode, 0)

    let addCostsToMap costMap points path =
        let shouldAddCostToMap costMap (point, cost) =
            if costMap |> Map.containsKey point
               && costMap[point] |> fst <= cost then
                false
            else
                true

        let addCostToMap pathCost (point, cost) = pathCost |> Map.add point (cost, path)

        points
        |> List.filter (shouldAddCostToMap costMap)
        |> List.fold addCostToMap costMap

    let rec step (costMap: Map<string, int * (string list)>) visited (queue: PriorityQueue<Node, int>) =
        match queue.Count with
        | 0
        | _ when visited |> Set.contains target ->
            target :: (costMap[target] |> snd)
            |> List.rev
            |> List.skip 1
        | _ ->
            let point = queue.Dequeue()
            let (Node (name, _, edges)) = point

            if visited |> Set.contains name then
                step costMap visited queue
            else
                let cost, path = costMap[name]

                let adjacentWithCost =
                    edges
                    |> List.filter (fun p -> visited |> Set.contains p |> not)
                    |> List.map (fun p -> (p, cost + 1))

                let newPathCost = addCostsToMap costMap adjacentWithCost (name :: path)
                let newVisisted = visited |> Set.add name

                adjacentWithCost
                |> List.iter (fun (point, cost) -> queue.Enqueue(getForName nodes point, cost))

                step newPathCost newVisisted queue


    let pathCost = [ start, (0, []) ] |> Map.ofList
    step pathCost Set.empty queue

let data = loadData ()


let positiveNodes =
    data
    |> List.filter (function
        | Node (_, flow, _) -> flow > 0)

let aaNode =
    data
    |> List.find (fun node -> Node.getName node = "AA")

let getPathAndDistance nodes =
    let path = findShortestPath data nodes
    path, path |> List.length

let allPaths =
    List.allPairs (aaNode :: positiveNodes) positiveNodes
    |> List.filter (fun (a, b) -> a <> b)
    |> List.map (fun (Node (a, _, _), Node (b, _, _)) -> a, b)
    |> List.map (fun nodes -> nodes, getPathAndDistance nodes)
    |> Map.ofList

type FlowState = { Time: int; Released: int; Flow: int }

module FlowState =
    let initialP1 = { Time = 30; Released = 0; Flow = 0 }

    let initialP2 = { Time = 26; Released = 0; Flow = 0 }

    let tick state =
        { state with
            Released = state.Released + state.Flow
            Time = state.Time - 1 }

    let increased increase state =
        { state with Flow = state.Flow + increase }

    let released { Released = released } = released

    let releasedAfterTime
        { Flow = flow
          Released = released
          Time = time }
        =
        released + (time * flow)

let getReleasedPressure nodes initial =
    let nodesByName =
        nodes
        |> List.map (fun node -> Node.getName node, node)
        |> Map.ofList

    let nodeNames = nodes |> List.map Node.getName

    let orderedByPotential state remaining current =
        let getPotential { Time = time } current target =
            let dist = allPaths[(current, target)] |> snd

            time
            - (dist + 1) * (nodesByName[target] |> Node.getFlow)

        remaining
        |> List.sortByDescending (getPotential state current)

    let getMaxRemainingPotential { Time = time } remaining =
        let getPotentialForIndex (idx, node) =
            let factor = time - ((idx * 2) + 1)

            if factor <= 0 then
                0
            else
                factor * (Node.getFlow node)

        let potentials =
            remaining
            |> List.map (fun name -> nodesByName[name])
            |> List.sortByDescending Node.getFlow
            |> List.indexed
            |> List.map getPotentialForIndex

        potentials |> List.sum

    let rec getBestReleasedPressure path currentBest state remaining current =
        let remaining = orderedByPotential state remaining current

        match (remaining, state) with
        | (_, { Time = time }) when time = 0 -> Some(FlowState.released state, path)
        | ([], _) -> getBestReleasedPressure path currentBest (FlowState.tick state) [] current
        | next :: rest, _ ->
            let getForNextNode currentBest next rest =
                let node = nodesByName[next]
                let _, dist = allPaths[(current, next)]

                if dist + 1 > state.Time then
                    Some(FlowState.releasedAfterTime state, path)
                else
                    let newState =
                        seq { 0..dist } // Includes the time to open the valve
                        |> Seq.fold (fun state _ -> FlowState.tick state) state
                        |> FlowState.increased (node |> Node.getFlow)

                    let maxRemainPotential = getMaxRemainingPotential state rest

                    if (FlowState.releasedAfterTime newState)
                       + maxRemainPotential < currentBest then
                        None
                    else
                        getBestReleasedPressure (current :: path) currentBest newState rest next


            let rec getForChildren currentBest beenDone toCheck toDo =
                let result =
                    getForNextNode currentBest toCheck (List.filter (fun n -> n <> toCheck) remaining)

                let newDone, newBest =
                    match result with
                    | None -> beenDone, currentBest
                    | Some released -> (released :: beenDone), (max currentBest (fst released))

                match toDo with
                | [] -> newDone
                | next :: rest -> getForChildren newBest newDone next rest

            match getForChildren currentBest [] next rest with
            | [] -> None
            | children ->
                let maxChild = children |> List.maxBy fst
                Some(maxChild)

    getBestReleasedPressure [] 0 initial nodeNames "AA"

getReleasedPressure positiveNodes FlowState.initialP1
|> Option.get
|> fst
|> printfn "Part 1: %A"

let rec splitIntoTwo nodes =
    match nodes with
    | next :: rest ->
        let children = splitIntoTwo rest

        children
        |> List.collect (fun (l1, l2) -> [ next :: l1, l2; l1, next :: l2 ])
    | [] -> [ ([], []) ]

let mutable i = 1

let runForPartitions (scores, released) (l1, l2) =
    if i % 1000 = 0 then printfn "%d" i
    i <- i + 1

    if Map.containsKey l2 scores then
        (scores, released)
    else
        match getReleasedPressure l1 FlowState.initialP2, getReleasedPressure l2 FlowState.initialP2 with
        | Some a, Some b -> (scores |> Map.add l1 (a, b), (a, b) :: released)
        | _ -> (scores, released)

splitIntoTwo positiveNodes
|> List.fold runForPartitions (Map.empty, [])
|> snd
|> List.maxBy (fun ((a, _), (b, _)) -> a + b)
|> (fun ((a, _), (b, _)) -> a + b)
|> printfn "Part 2: %d"
