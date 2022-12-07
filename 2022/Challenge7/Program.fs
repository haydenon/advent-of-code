open System

type FsItem =
    | Dir of string * FsItem list
    | File of string * int64

let rec parseList currentDir (entries: string list) =
    match entries with
    | next :: rest ->
        if next.StartsWith("$") then
            currentDir, entries
        else if next.StartsWith("dir") then
            parseList currentDir rest
        else
            let parts = next.Split(" ")
            let size = Int64.Parse parts[0]
            parseList (File(parts[1], size) :: currentDir) rest
    | [] -> currentDir, []

let rec parseTree (dirName, files) (entries: string list) =
    match entries with
    | entry :: rest ->
        if entry.StartsWith("$ cd") then
            let dir = entry.Split(" ")[2]

            if dir = ".." then
                Dir(dirName, files), rest
            else
                let subdir, rest = parseTree (dir, []) rest
                parseTree (dirName, subdir :: files) rest
        elif entry.StartsWith("$ ls") then
            let dirFiles, rest = parseList [] rest
            parseTree (dirName, List.append dirFiles files) rest
        else
            failwith "Invalid input"
    | [] -> Dir(dirName, files), []

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
