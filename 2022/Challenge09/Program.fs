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

let rec runInstructions (tailLocations) tcoords hcoords instructions =
    let rec runStep instruction tailLocations (tx, ty) (hx, hy) =
        let stepCount =
            match instruction with
            | Up d -> d
            | Down d -> d
            | Left d -> d
            | Right d -> d

        if stepCount = 0 then
            (tailLocations, (tx, ty), (hx, hy))
        else
            let (hx, hy) =
                match instruction with
                | Up _ -> (hx, hy - 1)
                | Down _ -> (hx, hy + 1)
                | Left _ -> (hx - 1, hy)
                | Right _ -> (hx + 1, hy)

            let distance = max (abs (hx - tx)) (abs (hy - ty))

            let ((tx, ty), tailLocations) =
                if distance >= 2 then
                    let tx, ty =
                        match instruction with
                        | Up _ -> (hx, hy + 1)
                        | Down _ -> (hx, hy - 1)
                        | Left _ -> (hx + 1, hy)
                        | Right _ -> (hx - 1, hy)

                    ((tx, ty), (tx, ty) :: tailLocations)

                else
                    ((tx, ty), tailLocations)

            let nextStep =
                match instruction with
                | Up _ -> Up(stepCount - 1)
                | Down _ -> Down(stepCount - 1)
                | Left _ -> Left(stepCount - 1)
                | Right _ -> Right(stepCount - 1)

            runStep nextStep tailLocations (tx, ty) (hx, hy)

    match instructions with
    | instruction :: rest ->
        let tailLocations, tcoords, hcoords =
            runStep instruction tailLocations tcoords hcoords

        runInstructions tailLocations tcoords hcoords rest
    | [] -> tailLocations

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> List.ofArray |> parseInstructions []


let data = loadData ()

let tailLocations = [ (0, 0) ]

data
// |> List.take 3
|> runInstructions tailLocations (0, 0) (0, 0)
|> Set.ofList
|> Set.count
|> printfn "%d"
