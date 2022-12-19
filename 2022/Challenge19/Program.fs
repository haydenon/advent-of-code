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
      Geode: int }

module State =
    let initialForTime time =
        { Time = time
          OreRobots = 1
          Ore = 0
          ClayRobots = 0
          Clay = 0
          ObsidianRobots = 0
          Obsidian = 0
          GeodeRobots = 1
          Geode = 0 }

    let canBuy state (_, costs) =
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
            | Obsidian -> { state with Obsidian = state.Obsidian + 1 }
            | Geode -> { state with GeodeRobots = state.Geode + 1 }

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


let rec getOptions costs toCheck options =
    match toCheck with
    | [] -> options
    | (state, buying) :: rest ->
        let canBuyForCosts = costs |> List.map (State.canBuy state >> not)
        printfn "%A" canBuyForCosts

        let newOptions =
            if canBuyForCosts |> List.exists id then
                (buying :: options)
            else
                options

        if canBuyForCosts |> List.forall id then
            getOptions costs rest newOptions
        else
            let canBuy = costs |> List.filter (State.canBuy state)

            let newToCheck =
                List.append
                    (canBuy
                     |> List.map (fun (res, costs) -> (state |> State.chargeForCost (res, costs), res :: buying)))
                    rest

            getOptions costs newToCheck newOptions

let rec getOptimalForBlueprint (costs: (Resource * ((Resource * int) list)) list) state =
    if state.Time = 0 then
        Some state.Obsidian
    else
        let opts = getOptions costs [ (state, []) ] []
        printfn "%A" opts

        let costsForRes resource =
            costs
            |> List.filter (fun (res, _) -> res = resource)
            |> List.head

        let getForNextOption (toBuy: Resource list) =
            let newState =
                toBuy
                |> List.map costsForRes
                |> List.fold State.purchase (state |> State.tick)

            getOptimalForBlueprint costs newState

        let rec getForChildren beenDone todo =
            match todo with
            | [] -> beenDone
            | opt :: rest -> getForChildren ((getForNextOption opt) :: beenDone) rest

        let children =
            getForChildren [] opts
            |> List.filter Option.isSome
            |> List.map Option.get

        match children with
        | [] -> None
        | values -> values |> List.max |> Some


let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let (ore, clay, obs, geod) =
        seq { 0 .. ((Array.length text) / 6) }
        |> Seq.map (fun i ->
            text
            |> Array.skip (i * 6)
            |> Array.take 5
            |> parseBlueprint)
        |> Seq.head

    [ (Ore, ore)
      (Clay, clay)
      (Obsidian, obs)
      (Geode, geod) ]

let costs = loadData ()

let state = State.initialForTime 10

let st =
    { state with
        Time = 2
        Ore = 8
        Clay = 20
        Obsidian = 7 }

getOptimalForBlueprint costs st |> printfn "%A"
// data |> printfn "%A"
