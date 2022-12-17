open System

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
let stackHeight = 6000

let createStack () =
    Array.init 7 (fun _ -> Array.init stackHeight (fun _ -> false))


let getStackHeight stack =
    stack
    |> Array.map (fun col ->
        col
        |> Array.tryFindIndexBack id
        |> function
            | Some num -> num
            | None -> 0)
    |> Array.max
    |> ((+) 1)

let printStack falling stack =
    let fallingCoords =
        match falling with
        | None -> Set.empty
        | Some (rock, (x, y)) ->
            let height = rock |> Array.length
            let width = rock[0] |> Array.length

            Seq.allPairs (seq { x .. (x + width - 1) }) (seq { y .. (y + height - 1) })
            |> Seq.filter (fun (xx, yy) -> rock[yy - y][xx - x])
            |> Set.ofSeq

    let height = getStackHeight stack

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

let runRockFalls initialCount pattern =
    let stack = createStack ()
    let patternLength = Array.length pattern

    let hasStackValueAt row col =
        if row < 0 then
            true
        else
            stack[col][row]

    let isValidLoc (x, y) (rock: bool [] []) =
        let length = rock[0] |> Array.length
        let inBounds = x >= 0 && x + length <= 7

        if inBounds then
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

        let placeRow row =
            let rockRow = rock[row - y]

            seq { x .. (x + length - 1) }
            |> Seq.iter (fun cx ->
                if rockRow[cx - x] then
                    stack[cx][row] <- true)

        seq { y .. (y + height - 1) } |> Seq.iter placeRow

    let rec runRock count patternIdx loc =
        let rock = rockPatterns[(initialCount - count) % 5]
        let nextStep = pattern[patternIdx % patternLength]

        let gustLoc =
            match nextStep, loc with
            | Right, (x, y) ->

                (x + 1, y)
            | Left, (x, y) ->

                (x - 1, y)

        let (locx, locy) =
            if isValidLoc gustLoc rock then
                gustLoc
            else
                loc

        let dropLoc = (locx, locy - 1)

        if isValidLoc dropLoc rock then

            runRock count (patternIdx + 1) dropLoc
        else
            placeRock rock (locx, locy)

            let newCount = count - 1

            if newCount = 0 then
                stack
            else
                let topHeight = getStackHeight stack
                runRock newCount (patternIdx + 1) (2, topHeight + 3)

    runRock initialCount 0 (2, 3)

let data = loadData ()

let res = runRockFalls 2022 data

res |> getStackHeight |> printfn "Part 1: %d"
