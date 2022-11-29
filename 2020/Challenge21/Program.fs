open System

let parseLine (line : string) =
  let allergenText = " (contains "
  let allergenLength = allergenText.Length
  let allergenIndex = line.IndexOf(allergenText)
  let ingredients = line.Substring(0, allergenIndex).Split(" ") |> Array.toList
  let allergens = line.Substring(allergenIndex + allergenLength, line.Length - (allergenIndex + allergenLength) - 1)
  let allergens = allergens.Split(", ") |> Array.toList
  (ingredients, allergens)

let rec parseData foods lines =
  match lines with
  | [] -> foods
  | line :: tail -> parseData (parseLine line :: foods) tail

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.toList
  |> parseData []

let getPossibleAllergens (values : string list list) =
  values
  |> List.map (Set.ofList)
  |> Set.intersectMany

let ingredientsByAllergen foods =
  foods
  |> List.collect (fun (ingredients, allergens) -> allergens |> List.map (fun a -> (a, ingredients)))
  |> List.groupBy fst
  |> List.map (fun (allergen, ls) -> (allergen, ls |> List.map snd))

let allIngredients foods =
  foods
  |> List.collect fst

let rec pairAllergens allergenPairs allergenMap =
  let determinate = allergenMap |> List.filter (snd >> List.length >> ((=) 1))
  let determinedIngredients = determinate |> List.collect snd
  let determinatePairs =
    determinate
    |> List.map (fun (a, ls) -> (a, List.head ls))
  let allergenPairs = determinatePairs @ allergenPairs
  let updMap =
    allergenMap
    |> List.filter (fun a -> not(List.contains a determinate))
    |> List.map (fun (a, ls) -> (a, List.filter (fun ing -> not(List.contains ing determinedIngredients)) ls))
  if List.isEmpty updMap
  then allergenPairs
  else pairAllergens allergenPairs updMap

[<EntryPoint>]
let main _ =
  let data = loadData()
  let allergensToIngredients =
    ingredientsByAllergen data
    |> List.map (fun (allergen, ing) -> (allergen, getPossibleAllergens ing |> Set.toList))
  let possibleAllergens =
    allergensToIngredients
    |> List.collect snd
    |> Set.ofList
  allIngredients data
  |> List.filter (fun ing -> not(Set.contains ing possibleAllergens))
  |> List.length
  |> printfn "Non allergenic ingredients appear %d times"

  printfn "Allergen list:"
  pairAllergens [] allergensToIngredients
  |> List.sortBy fst
  |> fun values -> String.Join(',', values |> List.map snd)
  |> printfn "%s"
  0