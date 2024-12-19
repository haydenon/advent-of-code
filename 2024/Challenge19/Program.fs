open System
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

let invalidSubstings = new HashSet<string>()
let validSubstringCompositions = new Dictionary<string, string list>()

let rec findComposition (components: string array) (text: string) =
    if invalidSubstings.Contains text then
        None
    else if validSubstringCompositions.ContainsKey text then
        Some(validSubstringCompositions.Item text)
    else if text.Length = 0 then
        Some []
    else
        let res =
            components
            |> Array.tryPick (fun p ->
                if text.StartsWith(p) then
                    match findComposition components (text.Substring(p.Length, text.Length - p.Length)) with
                    | Some comp ->
                        let newComp = p :: comp
                        validSubstringCompositions.Add(text, newComp)
                        Some newComp
                    | None -> None
                else
                    None)

        match res with
        | Some comp -> Some comp
        | None ->
            invalidSubstings.Add text |> ignore
            None

let mutable i = 0

patterns
|> Array.choose (fun p ->
    printfn "%d" i
    i <- i + 1
    findComposition components p)
|> Array.length
|> printfn "%A"
