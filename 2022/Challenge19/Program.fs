open System
open System.Collections.Generic
open System.Threading.Tasks

type Resource =
    | Ore
    | Clay
    | Obsidian
    | Geode

let parseResources (resources: string) =
    let parseResName =
        function
        | "ore" -> Ore
        | "clay" -> Clay
        | "obsidian" -> Obsidian
        | _ -> failwith "Invalid input"

    let parseResource (resource: string) =
        match
            resource.Split(" ")
            |> Array.filter (String.IsNullOrWhiteSpace >> not)
            with
        | [| num; res |] -> (parseResName res, num |> Int32.Parse)
        | vals -> failwithf "Invalid input %A" vals

    resources.Split(" and ")
    |> Array.map parseResource
    |> Array.toList

let parseBlueprint (line: string) =
    let lines =
        line.Substring("Blueprint 1: ".Length).Split(".")
        |> Array.map (fun str -> str.Trim())

    let oreRobot =
        lines[0]
            .Substring("Each ore robot costs ".Length)
        |> parseResources

    let clayRobot =
        lines[1]
            .Substring("Each clay robot costs ".Length)
        |> parseResources

    let obsRobot =
        lines[2]
            .Substring("Each obsidian robot costs ".Length)
        |> parseResources

    let geodeRobot =
        lines[3]
            .Substring("Each geode robot costs ".Length)
        |> parseResources

    (oreRobot, clayRobot, obsRobot, geodeRobot)

type State =
    { Time: int
      OreRobots: int
      Ore: int
      ClayRobots: int
      Clay: int
      ObsidianRobots: int
      Obsidian: int
      GeodeRobots: int
      Geode: int
      Previous: State option }

module State =
    let initialForTime time =
        { Time = time
          OreRobots = 1
          Ore = 0
          ClayRobots = 0
          Clay = 0
          ObsidianRobots = 0
          Obsidian = 0
          GeodeRobots = 0
          Geode = 0
          Previous = None }

    let canBuy state (res, costs) =
        let satisfiesResource =
            function
            | (Ore, num) -> state.Ore >= num
            | (Clay, num) -> state.Clay >= num
            | (Obsidian, num) -> state.Obsidian >= num
            | _ -> failwith "Nothing costs geodes"

        costs |> List.forall satisfiesResource

    let chargeForCost (_, costs) state =
        let minus state =
            function
            | (Ore, num) -> { state with Ore = state.Ore - num }
            | (Clay, num) -> { state with Clay = state.Clay - num }
            | (Obsidian, num) -> { state with Obsidian = state.Obsidian - num }
            | _ -> failwith "Nothing costs geods"

        costs |> List.fold minus state

    let purchase state (res, costs) =
        let withRobot =
            match res with
            | Ore -> { state with OreRobots = state.OreRobots + 1 }
            | Clay -> { state with ClayRobots = state.ClayRobots + 1 }
            | Obsidian -> { state with ObsidianRobots = state.ObsidianRobots + 1 }
            | Geode -> { state with GeodeRobots = state.GeodeRobots + 1 }

        withRobot |> chargeForCost (res, costs)

    let tick state =
        { state with
            Time = state.Time - 1
            Ore = state.Ore + state.OreRobots
            Clay = state.Clay + state.ClayRobots
            Obsidian = state.Obsidian + state.ObsidianRobots
            Geode = state.Geode + state.GeodeRobots }

    let key state =
        [| state.Ore
           state.OreRobots
           state.Clay
           state.ClayRobots
           state.Obsidian
           state.ObsidianRobots
           state.Geode
           state.GeodeRobots |]


type BlueprintItem = Resource * (Resource * int) list


type Actions =
    | Wait
    | Buy of Resource list


let rec getOptions (costs: (Resource * (Resource * int) list) list) state =
    let options = []

    let canGetRes =
        function
        | Ore -> state.OreRobots > 0
        | Clay -> state.ClayRobots > 0
        | Obsidian -> state.ObsidianRobots > 0
        | _ -> failwith "Geode is not a resource"

    let potentialToBuy =
        costs
        |> List.filter (fun (_, costs) -> costs |> List.forall (fst >> canGetRes))

    let canWaitToBuy =
        potentialToBuy
        |> List.exists (State.canBuy state >> not)

    let cantBuyAny = costs |> List.forall (State.canBuy state >> not)

    let newOptions =
        if canWaitToBuy then
            None :: options
        else
            options



    let maxRequired =
        [ Ore; Clay; Obsidian ]
        |> List.map (fun res ->
            costs
            |> List.collect (fun (_, depdendent) ->
                depdendent
                |> List.filter (fst >> ((=) res))
                |> List.map snd))
        |> List.map (List.max)

    let shouldBuy (res, _) =
        match res with
        | Ore -> state.OreRobots < maxRequired[0]
        | Clay -> state.ClayRobots < maxRequired[1]
        | Obsidian -> state.ObsidianRobots < maxRequired[2]
        | Geode -> true


    if cantBuyAny then
        newOptions
    else
        let canBuy =
            costs
            |> List.filter (fun cost -> State.canBuy state cost && shouldBuy cost)

        List.append newOptions (canBuy |> List.map (fst >> Some))

let getMaxGeodeCount (maxForTimeAndObs: Map<int * int, int>) costs state =
    let geodeCost = costs |> List.find (fun (res, _) -> res = Geode)

    // let obsCost =
    //     geodeCost
    //     |> snd
    //     |> List.find (fun (res, _) -> res = Obsidian)
    //     |> snd

    let remaining =
        if State.canBuy state geodeCost then
            state.Time - 1
        else
            state.Time - 2

    let maxFromBuilding =
        if
            maxForTimeAndObs
            |> Map.containsKey (state.Obsidian, state.Time)
        then
            maxForTimeAndObs[(state.Obsidian, state.Time)]
        else
        seq { 0..remaining }
        |> Seq.rev
        |> Seq.fold (fun built ind -> built + ind) 0

    state.Geode
    + state.GeodeRobots * state.Time
    + maxFromBuilding

let rec getOptimalForBlueprint
    (maxForTimeAndObs: Map<int * int, int>)
    highestGeodeState
    (costs: (Resource * ((Resource * int) list)) list)
    state
    =
    if state.Time = 0 then
        state
    elif highestGeodeState.Geode > getMaxGeodeCount maxForTimeAndObs costs state then
        highestGeodeState
    else
        let opts = getOptions costs state

        let costsForRes resource =
            costs
            |> List.filter (fun (res, _) -> res = resource)
            |> List.head

        let getForNextOption highestCount (toBuy: Resource list) =
            let newState =
                toBuy
                |> List.map costsForRes
                |> List.fold State.purchase (state |> State.tick)

            let newState = { newState with Previous = Some state }

            getOptimalForBlueprint maxForTimeAndObs highestCount costs newState

        let rec getForChildren highestState todo =
            match todo with
            | [] -> highestState
            | opt :: rest ->
                let opts =
                    match opt with
                    | Some opt -> [ opt ]
                    | None -> []

                let highestState =
                    match getForNextOption highestState opts with
                    | childState when childState.Geode > highestState.Geode -> childState
                    | _ -> highestState

                getForChildren highestState rest

        let res = getForChildren highestGeodeState opts
        // if not(bestForState.ContainsKey stateKey) || bestForState[stateKey] |> fst < state.Time then
        //   bestForState[stateKey] <- (state.Time, res)
        res

let generateMaxForTimeAndObs costs : Map<int * int, int> =
    let geodeCost = costs |> List.find (fun (res, _) -> res = Geode)

    let obsCost =
        geodeCost
        |> snd
        |> List.find (fun (res, _) -> res = Obsidian)
        |> snd

    let stateForTime obsRobots time =
        { Time = time
          OreRobots = 0
          Ore = Int32.MaxValue
          ClayRobots = 0
          Clay = Int32.MaxValue
          Obsidian = obsCost
          ObsidianRobots = obsRobots
          Geode = 0
          GeodeRobots = 0
          Previous = None }

    Seq.allPairs (seq { 0..20 }) (seq { 1..20 })
    |> Seq.fold
        (fun map (obs, time) ->
            let maxState =
                stateForTime obs time
                |> getOptimalForBlueprint Map.empty (State.initialForTime time) costs

            map |> Map.add (obs, time) maxState.Geode)
        Map.empty

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let parseCosts (line: string) =
        let (ore, clay, obs, geod) = parseBlueprint line

        [ (Ore, ore)
          (Clay, clay)
          (Obsidian, obs)
          (Geode, geod) ]

    text |> Array.map parseCosts



let costs = loadData ()

let state = State.initialForTime 24


let res =
    costs
    |> Array.mapi (fun idx costs ->
        // let map = generateMaxForTimeAndObs costs
        let res = getOptimalForBlueprint Map.empty state costs state
        printfn "Done %d" idx
        res)

res
|> Array.indexed
|> Array.map (fun (i, state) -> (i + 1) * state.Geode)
|> Array.sum
|> printfn "Part 1: %d"


let state2 = State.initialForTime 32

let res2 =
    costs
    |> Array.take 3
    |> Array.mapi (fun idx costs ->
        let map = generateMaxForTimeAndObs costs
        printfn "Starting %d" idx
        let res = getOptimalForBlueprint map state2 costs state2
        printfn "Done %d" idx
        res)

res2
|> Array.map (fun state -> state.Geode)
|> Array.fold (*) 1
|> printfn "Part 2: %d"
