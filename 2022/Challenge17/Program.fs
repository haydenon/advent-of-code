open System
open System.Collections.Generic

type Dir =
    | Left
    | Right

let parsePattern (line: string) =
    line.ToCharArray()
    |> Array.map (fun ch ->
        if ch = '>' then Right
        elif ch = '<' then Left
        else failwith "Invalid input")

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> Array.head |> parsePattern

let rockPatterns =
    [| [| [| true; true; true; true |] |]
       [| [| false; true; false |]
          [| true; true; true |]
          [| false; true; false |] |]
       [| [| true; true; true |]
          [| false; false; true |]
          [| false; false; true |] |]
       [| [| true |]
          [| true |]
          [| true |]
          [| true |] |]
       [| [| true; true |]; [| true; true |] |] |]

[<Literal>]
let stackHeight = 1000000

let createStack () =
    Array.init 7 (fun _ -> Array.init stackHeight (fun _ -> false))


let printStack falling (stack: bool [] [], heights) =
    let fallingCoords =
        match falling with
        | None -> Set.empty
        | Some (rock, (x, y)) ->
            let height = rock |> Array.length
            let width = rock[0] |> Array.length

            Seq.allPairs (seq { x .. (x + width - 1) }) (seq { y .. (y + height - 1) })
            |> Seq.filter (fun (xx, yy) -> rock[yy - y][xx - x])
            |> Set.ofSeq

    let height = heights |> Array.max

    let checkRow row idx =
        if fallingCoords |> Set.contains (idx, row) then
            '@'
        else if stack[idx][row] then
            '#'
        else
            '.'

    let printRow row =
        let chars = seq { 0..6 } |> Seq.map (checkRow row)
        printfn "|%s|" (new String(chars |> Seq.toArray))


    seq { 0 .. height + 4 }
    |> Seq.rev
    |> Seq.iter printRow

    printfn "+-------+"

let getStringRepresentation rockIdx patternIdx (stack: bool [] [], heights) =
    let top = heights |> Array.max

    if top < 50 then
        ""
    else
        let points =
            Seq.allPairs (seq { 0..6 }) (seq { (top - 49) .. top })
            |> Seq.map (fun (x, y) -> if stack[x][y] then "Y" else "N")
            |> Seq.toList

        let strs = List.append [ string rockIdx; string patternIdx ] points
        String.Join("", strs)

let runRockFalls (initialCount : int64) pattern =
    let stack = createStack ()
    let patternLength = Array.length pattern

    let heights = Array.init 7 (fun _ -> 0)

    let hasStackValueAt row col =
        if row < 0 then
            true
        else
            stack[col][row]

    let isValidLoc (x, y) (rock: bool [] []) =
        let length = rock[0] |> Array.length
        let inBounds = x >= 0 && x + length <= 7

        if inBounds then
            // if x + length = 7 then
            //   printfn "HERE"
            let rowNoCollisions (row, values) =
                values
                |> Array.indexed
                |> Array.forall (fun (idx, present) ->
                    not present
                    || not (hasStackValueAt (y + row) (x + idx)))

            rock
            |> Array.indexed
            |> Array.forall rowNoCollisions
        else
            false

    let placeRock (rock: bool [] []) (x, y) =
        let length = rock[0] |> Array.length
        let height = rock |> Array.length

        let placePoint x y =
            if heights[x] <= y then
                heights[x] <- y + 1

            stack[x][y] <- true

        let placeRow row =
            let rockRow = rock[row - y]

            seq { x .. (x + length - 1) }
            |> Seq.iter (fun cx ->
                if rockRow[cx - x] then
                    placePoint cx row)

        seq { y .. (y + height - 1) } |> Seq.iter placeRow

    let mutable maxDiff = 0

    let previousStates = Dictionary<string, int>()
    let heightsAtTurn = List<int>()

    let rec runRock rockTurns count patternIdx loc =
        let countIdx = (initialCount - count)
        let rockIdx = (int countIdx) % 5
        let rock = rockPatterns[rockIdx]
        let nextStep = pattern[patternIdx]

        let stringRepresentation =
            getStringRepresentation rockIdx patternIdx (stack, heights)

        if rockTurns = 0 && previousStates.ContainsKey(stringRepresentation) then
            // printStack None (stack, heights)
            let currentHeight = heights |> Array.max |> int64
            let oldIdx = (previousStates[stringRepresentation])
            let remaining = initialCount - (initialCount - count)
            let countDiff = (int countIdx) - oldIdx
            let periodDiff = int64 (currentHeight - int64 (heightsAtTurn[oldIdx]))
            let remainder = int (remaining % int64 countDiff)
            let remainderDiff = int64(heightsAtTurn[oldIdx + remainder] - heightsAtTurn[oldIdx])
            let heightAtTarget = currentHeight + (remaining / int64 countDiff) * periodDiff + remainderDiff
            heightAtTarget
        else
            if rockTurns = 0 && stringRepresentation <> String.Empty then
                previousStates.Add(stringRepresentation, int countIdx)
            if rockTurns = 0 then
              heightsAtTurn.Add(heights |> Array.max)

            let gustLoc =
                match nextStep, loc with
                | Right, (x, y) -> (x + 1, y)
                | Left, (x, y) -> (x - 1, y)

            let (locx, locy) =
                if isValidLoc gustLoc rock then
                    gustLoc
                else
                    loc

            let dropLoc = (locx, locy - 1)

            if isValidLoc dropLoc rock then

                runRock (rockTurns + 1) count ((patternIdx + 1) % patternLength) dropLoc
            else
                placeRock rock (locx, locy)

                let newCount = count - 1L

                if newCount = 0 then
                    printStack None (stack, heights)
                    heights |> Array.max |> int64
                else
                    let topHeight = heights |> Array.max
                    let minHeight = heights |> Array.min
                    maxDiff <- max maxDiff (topHeight - minHeight)
                    runRock 0 newCount ((patternIdx + 1) % patternLength) (2, topHeight + 3)

    runRock 0 initialCount 0 (2, 3)

let data = loadData ()

runRockFalls 2022L data
|> printfn "Part 1: %d"

runRockFalls 1000000000000L data
|> printfn "Part 2: %d"
