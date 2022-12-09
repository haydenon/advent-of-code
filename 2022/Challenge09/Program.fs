open System

type Instruction =
    | Up of int
    | Down of int
    | Right of int
    | Left of int

let parseInstruction (line: string) : Instruction =
    let parts = line.Split(" ")
    let num = Int32.Parse parts[1]

    if line.StartsWith("U") then Up num
    elif line.StartsWith("D") then Down num
    elif line.StartsWith("R") then Right num
    else Left num

let rec parseInstructions instructions (lines: string list) =
    match lines with
    | line :: rest -> parseInstructions ((parseInstruction line) :: instructions) rest
    | [] -> List.rev instructions

let rec runInstructions (allTailLocations: (int * int) list list) (coords: (int * int) list) instructions =
    let rec runStep instruction (alltailLocations: (int * int) list list) coords =
        let stepCount =
            match instruction with
            | Up d -> d
            | Down d -> d
            | Left d -> d
            | Right d -> d

        if stepCount = 0 then
            (alltailLocations, coords)
        else
            let (hx, hy) = List.head coords

            let (hx, hy) =
                match instruction with
                | Up _ -> (hx, hy - 1)
                | Down _ -> (hx, hy + 1)
                | Left _ -> (hx - 1, hy)
                | Right _ -> (hx + 1, hy)

            let coords = coords |> List.updateAt 0 (hx, hy)

            let rec updatePair coordTailLocationsPair (firstx, firsty) tails =
                match tails with
                | [] ->
                    coordTailLocationsPair
                    |> List.rev
                    |> (fun ls -> (List.map fst ls, List.map snd ls))
                | ((secx, secy), tailLocations) :: rest ->
                    let distance = max (abs (firstx - secx)) (abs (firsty - secy))

                    let pair =
                        if distance >= 2 then
                            let secx, secy =
                                if secx = firstx then
                                    (secx,
                                     (if firsty > secy then
                                          (firsty - 1)
                                      else
                                          (firsty + 1)))
                                elif secy = firsty then
                                    ((if firstx > secx then
                                          (firstx - 1)
                                      else
                                          (firstx + 1)),
                                     secy)
                                elif firstx - secx > 0 && firsty - secy > 0 then
                                    (secx + 1, secy + 1)
                                elif firstx - secx < 0 && firsty - secy > 0 then
                                    (secx - 1, secy + 1)
                                elif firstx - secx > 0 && firsty - secy < 0 then
                                    (secx + 1, secy - 1)
                                else
                                    (secx - 1, secy - 1)

                            ((secx, secy), (secx, secy) :: tailLocations)

                        else
                            ((secx, secy), tailLocations)

                    updatePair (pair :: coordTailLocationsPair) (fst pair) rest

            let head = List.head coords

            let tailsWithLocations =
                alltailLocations
                |> List.zip (coords |> List.skip 1)

            let (tailCoords, allTailLocations) = updatePair [] head tailsWithLocations

            let nextStep =
                match instruction with
                | Up _ -> Up(stepCount - 1)
                | Down _ -> Down(stepCount - 1)
                | Left _ -> Left(stepCount - 1)
                | Right _ -> Right(stepCount - 1)

            runStep nextStep allTailLocations (head :: tailCoords)

    match instructions with
    | instruction :: rest ->
        let tailLocations, coords = runStep instruction allTailLocations coords

        runInstructions tailLocations coords rest
    | [] ->
        allTailLocations

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> List.ofArray |> parseInstructions []


let data = loadData ()

let tailLocations1 = (List.init 1 (fun _ -> [ (0, 0) ]))

data
|> runInstructions tailLocations1 [ (0, 0); (0, 0) ]
|> List.head
|> Set.ofList
|> Set.count
|> printfn "Part 1: %d"


let tailLocations2 = (List.init 9 (fun _ -> [ (0, 0) ]))

data
|> runInstructions tailLocations2 (List.init 10 (fun _ -> (0, 0)))
|> List.skip 8
|> List.head
|> Set.ofList
|> Set.count
|> printfn "Part 2: %d"
