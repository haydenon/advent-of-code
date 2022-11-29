open System
open System.Collections.Generic

let isRule (line : string) =
  not(String.IsNullOrWhiteSpace line) && Char.IsDigit line.[0]

let isMessage (line : string) =
  not(String.IsNullOrWhiteSpace line) && Char.IsLetter line.[0]

type Rule =
  | Complex of int[][]
  | Base of char

let parseRule (line : string) =
  let parts = line.Split(':')
  let addr = Int32.Parse parts.[0]
  let remainder = parts.[1].Trim()
  if remainder.[0] = '"'
  then (addr, Base remainder.[1])
  else
    let rules =
      remainder.Split('|')
      |> Array.map ((fun n -> n.Split(' ')) >> (Array.filter (String.IsNullOrWhiteSpace >> not)) >> (Array.map int32))
    (addr, Complex rules)

let rec parseInput rules messages lines  =
  match lines with
  | line :: tail when isRule line -> parseInput (parseRule line :: rules) messages tail
  | line :: tail when isMessage line -> parseInput rules (line :: messages) tail
  | _ :: tail -> parseInput rules messages tail
  | [] -> (rules |> Map.ofList, List.rev messages)

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.toList
  |> parseInput [] []

let checkMessageForRule (rules : Map<int, Rule>) rule (message : string) =
  let rec checkMessage rule (message : string) =
    let checkRuleSet (subrules : int[]) : int[] =
      let checkRule res (rule : int) =
        match res with
        | [||] -> [||]
        | opts ->
          opts
          |> Array.collect (fun i -> checkMessage rule (message.Substring(i)) |> Array.map ((+) i))
      subrules
      |> Array.fold checkRule [|0|]
    match rules.[rule] with
    | Base ch ->
      if message.Length > 0 && message.[0] = ch then [|1|] else [||]
    | Complex subrules ->
      subrules
      |> Array.collect checkRuleSet
  let lengths = checkMessage rule message
  Array.exists ((=) (message.Length)) lengths

[<EntryPoint>]
let main _ =
  let (rules, messages) = loadData()
  messages
  |> List.filter (checkMessageForRule rules 0)
  |> List.length
  |> printfn "%d"

  let upd =
    rules
    |> Map.change 8 (fun _ -> Some(Complex [| [|42|]; [| 42; 8|] |]))
    |> Map.change 11 (fun _ -> Some(Complex [| [|42; 31|]; [| 42; 11; 31|] |]))
  messages
  |> List.filter (checkMessageForRule upd 0)
  |> List.length
  |> printfn "%d"
  0