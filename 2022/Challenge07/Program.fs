open System

type FsItem =
    | Dir of string * FsItem list
    | File of string * int64


let (|Cd|_|) (line: string) =
    if line.StartsWith("$ cd") then
        let dir = line.Split(" ")[2]
        Some dir
    else
        None

let (|Ls|_|) (line: string) =
    if line.StartsWith("$ ls") then
        Some()
    else
        None

let rec parseList currentDir (entries: string list) =
    match entries with
    | next :: rest ->
        if next.StartsWith("$") then
            currentDir, entries
        elif next.StartsWith("dir") then
            parseList currentDir rest
        else
            let parts = next.Split(" ")
            let size = Int64.Parse parts[0]
            parseList (File(parts[1], size) :: currentDir) rest
    | [] -> currentDir, []

let rec parseTree (dirName, files) (entries: string list) =
    match entries with
    | [] -> Dir(dirName, files), []
    | (Cd dir) :: rest ->
        if dir = ".." then
            Dir(dirName, files), rest
        else
            let subdir, rest = parseTree (dir, []) rest
            parseTree (dirName, subdir :: files) rest
    | (Ls) :: rest ->
        let dirFiles, rest = parseList [] rest
        parseTree (dirName, List.append dirFiles files) rest
    | _ -> failwith "Invalid input"

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text
    |> List.ofArray
    |> List.skip 1
    |> parseTree ("/", [])
    |> fst

let rec findDirectorySizesWhere pred fsItem =
    match fsItem with
    | Dir (_, children) ->
        let childSmallerThanValues =
            children
            |> List.map (findDirectorySizesWhere pred)

        let sum = childSmallerThanValues |> List.map fst |> List.sum
        let smallerThan = childSmallerThanValues |> List.collect snd

        (sum,
         if pred sum then
             sum :: smallerThan
         else
             smallerThan)
    | File (_, size) -> (size, [])

let data = loadData ()

data
|> findDirectorySizesWhere (fun size -> size < 100000L)
|> snd
|> List.sum
|> printfn "Part 1: %d"

let sum =
    findDirectorySizesWhere (fun _ -> true) data
    |> fst

let toDelete = 30000000L - (70000000L - sum)

data
|> findDirectorySizesWhere (fun size -> size > toDelete)
|> snd
|> List.min
|> printfn "Part 2: %d"
