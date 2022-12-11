open System

type Test =
    { Check: int
      IfTrue: int
      IfFalse: int }

type Operand =
    | Num of uint64
    | Old

type Operation =
    | Plus of Operand * Operand
    | Mult of Operand * Operand

type MonkeyState =
    { Items: uint64 list
      Op: Operation
      Test: Test }

let rec parseMonkeyState (lines: string list) =
    let monkeyLines = lines |> List.take 6

    let parseTest (test: string list) =
        let check =
            test[0]
                .Substring("  Test: divisible by ".Length)
            |> Int32.Parse

        let ifTrue =
            test[1]
                .Substring("    If true: throw to monkey ".Length)
            |> Int32.Parse

        let ifFalse =
            test[2]
                .Substring("    If false: throw to monkey ".Length)
            |> Int32.Parse

        { Check = check
          IfTrue = ifTrue
          IfFalse = ifFalse }

    let parseOp (op: string) =
        let getOperand operand =
            if operand = "old" then
                Old
            else
                Num(UInt64.Parse operand)

        let parts = op.Split(" ")
        let left, right = (getOperand parts[0], getOperand parts[2])

        if parts[1] = "*" then
            Mult(left, right)
        elif parts[1] = "+" then
            Plus(left, right)
        else
            failwith "Invalid input"

    let startingItems =
        monkeyLines[1]
            .Substring("  Starting items: ".Length)
            .Split(", ")
        |> Array.map UInt64.Parse
        |> Array.toList

    let operation =
        monkeyLines[2]
            .Substring("  Operation: new = ".Length)
        |> parseOp

    let test = lines |> List.skip 3 |> parseTest

    { Items = startingItems
      Op = operation
      Test = test }

let rec parseMonkeyStates monkeys (lines: string list) =
    match lines with
    | [] -> monkeys |> List.rev |> List.toArray
    | line :: _ when line.StartsWith("Monkey") ->
        parseMonkeyStates (parseMonkeyState lines :: monkeys) (lines |> List.skip 6)
    | _ :: rest -> parseMonkeyStates monkeys rest

let getNewWorriedLevel (old : uint64) op =
    let getValue =
        function
        | Old -> old
        | Num num -> num

    match op with
    | Mult (left, right) -> getValue left * getValue right
    | Plus (left, right) -> getValue left + getValue right

let addToMonkey (monkeys: MonkeyState []) idx item =
    monkeys
    |> Array.updateAt idx { monkeys[idx] with Items = List.append monkeys[idx].Items [ item ] }

let setItemsOnMonkey (monkeys: MonkeyState []) idx items =
    monkeys
    |> Array.updateAt idx { monkeys[idx] with Items = items }

let rec runMonkey (worryOp : uint64 -> uint64) (inspected: int64 []) idx (monkeys: MonkeyState []) =
    if idx >= Array.length monkeys then
        monkeys
    else
        match monkeys[idx] with
        | { Items = [] } -> runMonkey worryOp inspected (idx + 1) monkeys
        | { Items = next :: rest
            Op = op
            Test = test } ->
            let monkeys = setItemsOnMonkey monkeys idx rest
            let outcome = getNewWorriedLevel next op |> worryOp

            let toThrowTo =
                if outcome % (uint64 test.Check) = 0UL then
                    test.IfTrue
                else
                    test.IfFalse

            inspected[idx] <- inspected[idx] + 1L

            addToMonkey monkeys toThrowTo outcome
            |> runMonkey worryOp inspected idx

let runRound worryOp (monkeys, inspected) round = (runMonkey worryOp inspected 0 monkeys, inspected)

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> Array.toList |> parseMonkeyStates []


let data = loadData ()

let worryOp1 op = op / 3UL

seq { 1..20 }
|> Seq.fold (runRound worryOp1) (data, Array.init (data |> Array.length) (fun _ -> 0L))
// |> fst
|> snd
|> Array.sortDescending
|> Array.take 2
|> Array.reduce (*)
|> printfn "Part 1: %d"


let common =
  data
  |> Array.map (fun { Test = { Check = check } }  -> check)
  |> Array.reduce (*)
  |> uint64

let worryOp2 op =
  let times = op / common
  op - (common * times)

seq { 1..10000 }
|> Seq.fold (runRound worryOp2) (data, Array.init (data |> Array.length) (fun _ -> 0L))
|> snd
|> Array.sortDescending
|> Array.take 2
|> Array.reduce (*)
|> printfn "Part 2: %d"