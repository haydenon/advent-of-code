﻿open System
open System.Collections.Generic

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    let splitIndex =
        text
        |> Array.findIndex (fun line -> String.IsNullOrWhiteSpace(line))

    let components =
        text
        |> Array.take splitIndex
        |> Array.collect (fun line -> line.Split(", "))

    let patterns = text |> Array.skip (splitIndex + 1)

    components, patterns

let (components, patterns) = loadData ()

let cache = new Dictionary<string, Option<int64>>()

let rec findCompositions (components: string array) (text: string) =
    if cache.ContainsKey text then
        cache.Item text
    else if text.Length = 0 then
        Some 1
    else
        let res =
            components
            |> Array.map (fun p ->
                if text.StartsWith(p) then
                    findCompositions components (text.Substring(p.Length, text.Length - p.Length))
                    |> Option.map (fun comp -> comp, p)
                else
                    None)

        let allComps =
            res
            |> Array.fold
                (fun acc r ->
                    match acc, r with
                    | None, Some (comp, p) -> Some(comp)
                    | Some (comps), Some (comp, p: string) -> Some(comps + comp)
                    | Some comps, None -> Some comps
                    | _ -> None)
                None

        cache.Add(text,allComps)
        allComps

patterns
|> Array.choose (findCompositions components)
|> Array.length
|> printfn "Part 1: %d"


patterns
|> Array.choose (findCompositions components)
|> Array.sum
|> printfn "Part 2: %d"
