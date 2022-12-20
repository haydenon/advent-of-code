open System

type Resource =
    | Ore
    | Clay
    | Obsidian
    | Geode

let parseResources (resources: string) =
    let resources = resources.Substring(0, resources.Length - 1)

    let parseResName =
        function
        | "ore" -> Ore
        | "clay" -> Clay
        | "obsidian" -> Obsidian
        | _ -> failwith "Invali input"

    let parseResource (resource: string) =
        match resource.Split(" ") with
        | [| num; res |] -> (parseResName res, num |> Int32.Parse)
        | _ -> failwith "Invalid input"

    resources.Split(" and ")
    |> Array.map parseResource
    |> Array.toList

let parseBlueprint (lines: string []) =
    let oreRobot =
        lines[1]
            .Substring("  Each ore robot costs ".Length)
        |> parseResources

    let clayRobot =
        lines[2]
            .Substring("  Each clay robot costs ".Length)
        |> parseResources

    let obsRobot =
        lines[3]
            .Substring("  Each obsidian robot costs ".Length)
        |> parseResources

    let geodeRobot =
        lines[4]
            .Substring("  Each geode robot costs ".Length)
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

    let shouldBuy (res, _) =
        let max = 6

        match res with
        | Ore -> state.OreRobots < max
        | Clay -> state.ClayRobots < max
        | Obsidian -> state.ObsidianRobots < max
        | Geode -> true


    if cantBuyAny then
        newOptions
    else
        let canBuy =
            costs
            |> List.filter (fun cost -> State.canBuy state cost && shouldBuy cost)

        List.append newOptions (canBuy |> List.map (fst >> Some))

let getMaxGeodeCount costs state =
    let geodeCost = costs |> List.find (fun (res, _) -> res = Geode)

    let remaining =
        if State.canBuy state geodeCost then
            state.Time - 1
        else
            state.Time - 2

    let maxFromBuilding =
        seq { 0..remaining }
        |> Seq.rev
        |> Seq.fold (fun built ind -> built + ind) 0

    state.Geode
    + state.GeodeRobots * state.Time
    + maxFromBuilding


let rec getOptimalForBlueprint highestGeodeState (costs: (Resource * ((Resource * int) list)) list) state =
    if state.Time = 0 then
        state
    elif highestGeodeState.Geode > getMaxGeodeCount costs state then
        // printfn "%A" (getMaxGeodeCount costs state)
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

            getOptimalForBlueprint highestCount costs newState

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

        getForChildren highestGeodeState opts

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let (ore, clay, obs, geod) =
        seq { 0 .. ((Array.length text) / 6) }
        |> Seq.map (fun i ->
            text
            |> Array.skip (i * 6)
            |> Array.take 5
            |> parseBlueprint)
        |> Seq.skip 1
        |> Seq.head

    [ (Ore, ore)
      (Clay, clay)
      (Obsidian, obs)
      (Geode, geod) ]

let costs = loadData ()

let state = State.initialForTime 24

getOptimalForBlueprint state costs state
|> printfn "%A"
