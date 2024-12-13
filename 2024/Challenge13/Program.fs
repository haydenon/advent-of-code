open System
open System.Text.RegularExpressions

let buttonRegex =
    Regex(@"Button (?:A|B): X\+([0-9]{1,5}), Y\+([0-9]{1,5})", RegexOptions.Compiled)

let prizeMatch = Regex(@"Prize: X=([0-9]{1,7}), Y=([0-9]{1,7})")

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let rec parseEntries lines entries =
        if lines |> Array.length < 3 then
            entries
        else
            let buttonA = buttonRegex.Match(lines[0])
            let buttonB = buttonRegex.Match(lines[1])
            let prize = prizeMatch.Match(lines[2])

            let toSkip = min (lines |> Array.length) 4

            let parseMatch (valueMatch: Match) =
                ((valueMatch.Groups[1].Value |> Int64.Parse, valueMatch.Groups[2].Value |> Int64.Parse))

            parseEntries
                (lines |> Array.skip toSkip)
                ((parseMatch buttonA, parseMatch buttonB, parseMatch prize)
                 :: entries)

    parseEntries text [] |> List.rev

let findBestSolution ((ax, ay), (bx, by), (prizeX, prizeY)) =
    let maxA = min (prizeX / ax) (prizeY / ay) |> min 100L
    let maxB = min (prizeX / bx) (prizeY / by) |> min 100L

    let solutions =
        Seq.allPairs (seq { 0L .. maxA }) (seq { 0L .. maxB })
        |> Seq.filter (fun (a, b) ->
            a * ax + b * bx = prizeX
            && a * ay + b * by = prizeY)
        |> Seq.map (fun (a, b) -> a * 3L + b)
        |> Seq.toList

    match solutions with
    | [] -> None
    | sol -> Some(sol |> List.min)

let solveCramer (equations: int64 array array) =
    let size = 2
    let matrix = Array.init size (fun r -> Array.init size (fun c -> equations[r][c]))
    let column = Array.init size (fun r -> equations[r][size])

    let solveForMatrix (matrix: int64 [] []) (column: int64 []) =
        let det columnIndex =
            let value row col =
                if col = columnIndex then
                    column[row]
                else
                    matrix[row][col]

            value 0 0 * value 1 1 - value 0 1 * value 1 0

        let determinant = det -1
        let answer = Array.init size (fun i -> det i / determinant)
        answer

    solveForMatrix matrix column

let solve ((ax, ay), (bx, by), (prizeX, prizeY)) =
    let offset = 10000000000000L
    let prizeX = prizeX + offset
    let prizeY = prizeY + offset

    let res =
        solveCramer [| [| ax; bx; prizeX |]
                       [| ay; by; prizeY |] |]

    let a = res[0]
    let b = res[1]

    match ax * a + bx * b = prizeX, ay * a + by * b = prizeY with
    | true, true -> Some(a * 3L + b)
    | _ -> None

let data = loadData ()

data
|> List.choose findBestSolution
|> List.sum
|> printfn "Part 1: %d"

data
|> List.choose solve
|> List.sum
|> printfn "Part 2: %d"
