open System

type Op =
  | Nop of int
  | Acc of int
  | Jmp of int

let parseLine (line : string) =
  let opArg = line.Split(" ")
  let arg = Int32.Parse opArg.[1]
  match opArg.[0] with
  | "nop" -> Nop arg
  | "jmp" -> Jmp arg
  | "acc" -> Acc arg
  | _ -> failwith "Invalid input"

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.map parseLine

let rec run (ops : Op[]) (visited : Set<int>) reg idx =
  if visited.Contains idx
  then reg
  else
    let updatedVisited = visited.Add idx
    match ops.[idx] with
    | Nop _   -> run ops updatedVisited reg (idx + 1)
    | Acc arg -> run ops updatedVisited (reg + arg) (idx + 1)
    | Jmp arg -> run ops updatedVisited reg (idx + arg)

let rec runWithSwitch (ops : Op[]) (visited : Set<int>) reg idx switchIdx  : int option =
  if visited.Contains idx then None
  else if idx >= Array.length ops then Some reg
  else
    let op =
      if idx = switchIdx
      then
        match ops.[idx] with
        | Nop arg -> Jmp arg
        | Jmp arg -> Nop arg
        | _ -> failwith "Cannot switch op"
      else ops.[idx]
    let updatedVisited = visited.Add idx
    match op with
    | Nop _   -> runWithSwitch ops updatedVisited reg (idx + 1) switchIdx
    | Acc arg -> runWithSwitch ops updatedVisited (reg + arg) (idx + 1) switchIdx
    | Jmp arg -> runWithSwitch ops updatedVisited reg (idx + arg) switchIdx

[<EntryPoint>]
let main _ =
  let data = loadData()
  let reg = run data Set.empty 0 0
  printfn "Register value is: %d" reg

  data
  |> Array.indexed
  |> Array.filter ((fun (_, op) -> op) >> (function Nop _ | Jmp _ -> true | _ -> false))
  |> Array.map (fun (idx, _) -> idx)
  |> Array.tryPick (runWithSwitch data Set.empty 0 0)
  |> Option.get
  |> printfn "Final register value when switching is: %d"
  0