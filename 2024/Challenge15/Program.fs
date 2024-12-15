open System

type Direction =
    | North
    | South
    | East
    | West

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let map =
        text
        |> Array.takeWhile (fun str -> not (String.IsNullOrWhiteSpace(str)))

    let dirs = text |> Array.skip ((map |> Array.length) + 1)

    let parseDirs (dirs: string array) =
        dirs
        |> Array.collect (fun line ->
            line
            |> Array.ofSeq
            |> Array.map (function
                | '^' -> North
                | 'v' -> South
                | '>' -> East
                | '<' -> West
                | _ -> failwith "Invalid"))
        |> Array.toList

    let parseMap (map: string array) = map |> Array.map Seq.toArray
    parseMap map, parseDirs dirs

let (board, dirs) = loadData ()

let printBoardEnabled1 = false
let printBoardEnabled2 = false

let printBoard (board: char array array) dir =
    board
    |> Array.iter (fun line -> printfn "%s" (String line))

    match dir with
    | Some d -> printfn "%A" d
    | _ -> ()

    printfn ""

let rec moveRobot (board: char array array) directions (x, y) =
    let moveInDirection dir =
        let (dx, dy) =
            match dir with
            | North -> (0, -1)
            | South -> (0, 1)
            | West -> (-1, 0)
            | East -> (1, 0)

        let addIncrement x y = x + dx, y + dy
        let (nx, ny) = addIncrement x y

        if board[ny][nx] = '.' then
            board[ny][nx] <- board[y][x]
            board[y][x] <- '.'
            (nx, ny)
        else if board[ny][nx] = 'O' then
            let rec getEnd (x, y) =
                match board[y][x] with
                | '#' -> None
                | 'O' -> getEnd (addIncrement x y)
                | '.' -> Some(x, y)
                | _ -> failwith "Invalid'"

            match getEnd (nx, ny) with
            | Some (ex, ey) ->
                board[ny][nx] <- board[y][x]
                board[y][x] <- '.'
                board[ey][ex] <- 'O'
                (nx, ny)
            | None -> (x, y)
        else
            x, y

    match directions with
    | [] -> board
    | next :: rest ->
        let nextCoords = moveInDirection next

        if printBoardEnabled1 then
            printBoard board (Some next)

        moveRobot board rest nextCoords

let start =
    Seq.allPairs (seq { 0 .. board[0].Length - 1 }) (seq { 0 .. board.Length - 1 })
    |> Seq.find (fun (x, y) -> board[y][x] = '@')

let scoreBoard ch (board: char array array) =
    Seq.allPairs (seq { 0 .. board[0].Length - 1 }) (seq { 0 .. board.Length - 1 })
    |> Seq.sumBy (fun (x, y) ->
        if board[y][x] = ch then
            x + y * 100
        else
            0)

let copy = board |> Array.map Array.copy

moveRobot copy dirs start
|> scoreBoard 'O'
|> printfn "%d"

let getPart2Board board =
    let getNewChars =
        function
        | '#' -> [ '#'; '#' ]
        | 'O' -> [ '['; ']' ]
        | '.' -> [ '.'; '.' ]
        | '@' -> [ '@'; '.' ]
        | _ -> failwith "Invalid"

    board
    |> Array.map (
        Array.toSeq
        >> Seq.collect getNewChars
        >> Array.ofSeq
    )

let joinMaps a b =
    a
    |> Map.toList
    |> List.fold
        (fun acc (key, value) ->
            acc
            |> Map.change key (function
                | Some '.'
                | None -> Some value
                | Some v -> Some v))
        b


let rec moveRobotPart2 (board: char array array) directions (x, y) =
    let moveInDirection dir =
        let (dx, dy) =
            match dir with
            | North -> (0, -1)
            | South -> (0, 1)
            | West -> (-1, 0)
            | East -> (1, 0)

        let addIncrement x y = x + dx, y + dy

        let rec moveInFront toMove (previous: char) (x, y) =
            match toMove |> Map.tryFind (x,y) with
            | Some _ -> Some toMove
            | None ->
              let next = (addIncrement x y)

              match board[y][x], dir with
              | '.', _ -> Some(toMove |> Map.add (x, y) previous)
              | '#', _ -> None
              | '[', d when d = North || d = South ->
                  let newToMove = (toMove |> Map.add (x, y) previous)

                  match moveInFront newToMove '[' next, moveInFront newToMove '.' (x + 1, y) with
                  | Some a, Some b -> Some(joinMaps a b |> Map.add (x, y) previous)
                  | _ -> None
              | ']', d when d = North || d = South ->
                  let newToMove = (toMove |> Map.add (x, y) previous)

                  match moveInFront newToMove ']' next, moveInFront newToMove '.' (x - 1, y) with
                  | Some a, Some b -> Some(joinMaps a b)
                  | _ -> None
              | '[', _
              | ']', _
              | '@', _ -> moveInFront (toMove |> Map.add (x, y) previous) (board[y][x]) next
              | _ -> failwith "Invalid"

        match moveInFront Map.empty '.' (x, y) with
        | Some (moves) ->
            moves
            |> Map.toList
            |> List.iter (fun ((x, y), ch) -> board[y][x] <- ch)

            (addIncrement x y)
        | None -> (x, y)

    match directions with
    | [] -> board
    | next :: rest ->
        let nextCoords = moveInDirection next

        if printBoardEnabled2 then
            printBoard board (Some next)

        moveRobotPart2 board rest nextCoords




let part2Board = getPart2Board board

let part2Start =
    Seq.allPairs (seq { 0 .. part2Board[0].Length - 1 }) (seq { 0 .. part2Board.Length - 1 })
    |> Seq.find (fun (x, y) -> part2Board[y][x] = '@')

moveRobotPart2 part2Board dirs part2Start
|> scoreBoard '['
|> printfn "%d"
