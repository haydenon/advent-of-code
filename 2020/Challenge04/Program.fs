open System
open System.Text.RegularExpressions

type Height =
  | Inches      of int
  | Centimetres of int
  | Invalid

type Field =
  | BirthYear  of int
  | IssueYear  of int
  | ExpiryYear of int
  | Height     of Height
  | HairColour of string
  | EyeColour  of string
  | PassportId of string
  | CountryId
  | Invalid

let parseHeight (value : string) =
  if value.EndsWith("cm") then Centimetres (Int32.Parse (value.Substring(0, value.Length - 2)))
  else if value.EndsWith("in") then Inches (Int32.Parse (value.Substring(0, value.Length - 2)))
  else Height.Invalid

let parseField (nameValue : string[]) =
  let fieldType = nameValue.[0]
  let value = nameValue.[1]
  match fieldType with
  | "byr" -> BirthYear (Int32.Parse value)
  | "iyr" -> IssueYear (Int32.Parse value)
  | "eyr" -> ExpiryYear (Int32.Parse value)
  | "hgt" -> Height (parseHeight value)
  | "hcl" -> HairColour value
  | "ecl" -> EyeColour value
  | "pid" -> PassportId value
  | "cid" -> CountryId
  | _ -> Invalid

let parsePassport values =
  let splitValue (value : string) = value.Split(':')
  values
  |> List.map (splitValue >> parseField)
  |> List.toArray

let getFields (line : string) =
  line.Split(' ')
  |> Array.toList

let rec loadPassports (input : string list) passports fields : Field[] list =
  match input with
  | line :: tail when line.Trim().Length > 0 ->
    loadPassports tail passports ((getFields line) @ fields)
  | _ :: tail ->
    loadPassports tail ((parsePassport fields) :: passports ) []
  | [] ->
    (parsePassport fields) :: passports

let loadData () =
  let lines =
    System.IO.File.ReadAllLines("./input.txt")
    |> List.ofArray
  loadPassports lines [] []

let required =
  [| "byr";
     "iyr";
     "eyr";
     "hgt";
     "hcl";
     "ecl";
     "pid" |]

let getFieldKey field =
  match field with
  | BirthYear _  -> "byr"
  | IssueYear _  -> "iyr"
  | ExpiryYear _ -> "eyr"
  | Height _     -> "hgt"
  | HairColour _ -> "hcl"
  | EyeColour _  -> "ecl"
  | PassportId _ -> "pid"
  | CountryId _  -> "cid"
  | _ -> ""

let hasRequiredPresent fields =
  let keys = Array.map getFieldKey fields
  required
  |> Array.forall(fun field -> Array.contains field keys)

let validHex hex = Regex.IsMatch(hex, "^#[a-f0-9]{6}$")

let validPassportId id = Regex.IsMatch(id, "^[0-9]{9}$")

let eyeColours = [|"amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|]

let isFieldValid field =
  match field with
  | BirthYear yr             -> yr >= 1920 && yr <= 2002
  | IssueYear yr             -> yr >= 2010 && yr <= 2020
  | ExpiryYear yr            -> yr >= 2020 && yr <= 2030
  | Height (Centimetres hgt) -> hgt >= 150 && hgt <= 193
  | Height (Inches hgt)      -> hgt >= 59 && hgt <= 76
  | HairColour col           -> validHex col
  | EyeColour col            -> Array.contains col eyeColours
  | PassportId id            -> validPassportId id
  | CountryId                -> true
  | _ -> false

let isValid fields =
  hasRequiredPresent fields && Array.forall isFieldValid fields

[<EntryPoint>]
let main _ =
  let data = loadData()
  data
  |> List.filter hasRequiredPresent
  |> List.length
  |> printfn "%d passports have all required fields"

  data
  |> List.filter isValid
  |> List.length
  |> printfn "%d passports are valid"
  0