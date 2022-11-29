open System
open System.Collections.Generic

type Bag = { Colour: string
             Holds: (string * int) list}

let split (delimeter : string) (str : string) = str.Split delimeter

let parseContents (contents : string) =
  let parseContent (str : string) =
    let a =
      str.Trim()
      |> split " "
      |> Array.take 3
    (sprintf "%s %s" a.[1] a.[2], Int32.Parse a.[0])
  contents.Split ","
  |> Array.map parseContent
  |> Array.toList

let parseLine (line : string) : Bag =
  let colourContains = split " bags contain " line
  let colour = colourContains.[0]
  let contents = colourContains.[1]
  let contents = contents.Substring(0, contents.Length - 1)
  if contents.EndsWith("no other bags") then { Colour = colour; Holds = [] }
  else { Colour = colour; Holds = parseContents contents }

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseLine

let isShinyGold (colour, _) = colour = "shiny gold"

let bagForColour bags bag = Array.find (fun b -> b.Colour = bag) bags

let populateBagCountHoldCount bags : Map<string, int> =
  let rec populateBagCount (countPerBag : Map<string, int>) bag  : Map<string, int> =
    if countPerBag.ContainsKey bag
    then countPerBag
    else
      let { Holds = holds } = Array.find (fun b -> b.Colour = bag) bags
      let goldBagCount =
        holds
        |> List.filter isShinyGold
        |> List.sumBy (fun (_, count) -> count)
      let updatedCounts =
        holds
        |> List.filter (isShinyGold >> not)
        |> List.fold (fun perBag (colour, _) -> populateBagCount perBag colour) countPerBag
      let childrenCount =
        holds
        |> List.filter (isShinyGold >> not)
        |> List.sumBy (fun (colour, count) -> updatedCounts.[colour] * count)
      updatedCounts.Add(bag, childrenCount + goldBagCount)
  bags
  |> Array.map (fun { Colour = colour } -> colour)
  |> Array.fold populateBagCount Map.empty

let containsCount bags countPredicate bag =
  let countPerBag = Dictionary<string, int>()
  let rec countForBag bag : int =
    if countPerBag.ContainsKey bag then countPerBag.[bag]
    else
      let { Holds = holds } = bagForColour bags bag
      let getCount colour count =
        if colour |> bagForColour bags |> countPredicate
        then ((countForBag colour) * count) + count
        else count
      let countNum =
        holds
        |> List.sumBy (fun (colour, count) -> getCount colour count)
      countPerBag.Add(bag, countNum)
      countNum
  countForBag bag


[<EntryPoint>]
let main _ =
  let data = loadData()
  let shinyBagHoldCount = populateBagCountHoldCount data
  shinyBagHoldCount
  |> Map.filter (fun _ c -> c > 0)
  |> Map.count
  |> printfn "%d bags can hold a shiny gold bag"

  let containedByGold =
    shinyBagHoldCount
    |> Map.filter (fun _ c -> c = 0)
    |> Map.toArray
    |> Array.map ((fun (colour, _) -> colour) >> bagForColour data)

  containsCount containedByGold (fun b -> not(List.isEmpty b.Holds)) "shiny gold"
  |> printfn "%d bags can be contained within a gold bag"
  0