open System

let rec findMarker count (characters: char list) idx (str: string) =
    if Set.count (Set.ofList characters) = count then
        idx + count
    else
        let newChars = str[0] :: List.take (count - 1) characters
        findMarker count newChars (idx + 1) (str.Substring 1)

let rec getInitialState count (characters: char list) idx (str: string) =
    if idx = count then
        characters, str
    else
        let newChars = str[0] :: characters
        getInitialState count newChars (idx + 1) (str.Substring(1))

let findMarkerIndexPart1 row =
    let chars, str = getInitialState 4 [] 0 row
    findMarker 4 chars 0 str

let findMarkerIndexPart2 row =
    let chars, str = getInitialState 14 [] 0 row
    findMarker 14 chars 0 str

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")
    text |> List.ofArray

let data = loadData ()

data
|> List.head
|> findMarkerIndexPart1
|> printfn "Part 1: %d"

data
|> List.head
|> findMarkerIndexPart2
|> printfn "Part 2: %d"
