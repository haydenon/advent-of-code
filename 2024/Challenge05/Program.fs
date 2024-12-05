open System
open System.Linq

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let index =
        text
        |> Array.findIndex (fun line -> String.IsNullOrWhiteSpace line)

    let parseOrdering (line: string) =
        let parts = line.Split("|") |> Array.map Int32.Parse
        (parts[0], parts[1])

    let parsePageUpdates (line: string) =
        line.Split(",")
        |> Array.map Int32.Parse
        |> Array.toList

    let orderings =
        text
        |> Array.take index
        |> Array.map parseOrdering
        |> Array.toList

    (orderings,
     text
     |> Array.skip (index + 1)
     |> Array.map parsePageUpdates
     |> Array.toList)

let getOrdering (orderRules: (int * int) list) : int list =
    let allItems =
        orderRules
        |> List.fold (fun acc (a, b) -> a :: b :: acc) []
        |> Set.ofList
        |> Set.toList

    let childToParentRules = orderRules |> List.map (fun (p, c) -> (c, p))

    let childToParents: Map<int, int list> =
        childToParentRules.ToLookup(fun (c, _) -> c)
        |> Seq.map (fun grp -> (grp.Key, grp |> Seq.map (fun (c, p) -> p) |> Seq.toList))
        |> Map.ofSeq

    let rec getNext ordering (ordered: Set<int>) remaining =
        let rec getHead () =
            childToParents
            |> Map.toList
            |> List.find (fun (c, p) ->
                not (ordered |> Set.contains c)
                && p
                   |> List.forall (fun i -> ordered |> Set.contains i))
            |> fst

        match remaining with
        | [] -> ordering |> List.rev
        | next :: _ ->
            let head = getHead ()
            let remaining = remaining |> List.except [ head ]
            let ordered = ordered |> Set.add head
            let ordering = head :: ordering
            getNext ordering ordered remaining

    let first =
        allItems
        |> List.find (fun i -> childToParents |> Map.containsKey i |> not)

    getNext [ first ] (Set.ofList [ first ]) (allItems |> List.except [ first ])

let data = loadData ()

let getOrder (item: int list) =
    let items = item |> Set.ofList

    data
    |> fst
    |> List.filter (fun (p, c) -> items |> Set.contains p && items |> Set.contains c)
    |> getOrdering

let updates = data |> snd

let updatedInOrder =
    updates
    |> List.map (fun list ->
        let order = getOrder list

        list
        |> List.sortBy (fun item -> order |> List.findIndex (fun ord -> ord = item)))
    |> List.toArray

let alreadyOrdered =
    updates
    |> List.indexed
    |> List.filter (fun (idx, list) -> updatedInOrder[idx] = list)
    |> List.map snd

let getMiddle list =
    let arr = list |> List.toArray
    let middle = (arr |> Array.length) / 2
    arr[middle]

alreadyOrdered
|> List.map getMiddle
|> List.sum
|> printfn "Part 1: %d"

let outOfOrder =
    updatedInOrder
    |> Array.toList
    |> List.except alreadyOrdered

outOfOrder
|> List.map getMiddle
|> List.sum
|> printfn "Part 2: %d"
