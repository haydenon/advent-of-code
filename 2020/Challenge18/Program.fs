open System

let loadData () = System.IO.File.ReadAllLines("./input.txt")

type Op = Plus | Times

type Token =
  | Num of int
  | Op of Op
  | OpenParen
  | CloseParen

type Expression =
  | Literal of int
  | Binary of Op * Expression * Expression

let (|Digit|_|) ch = if Char.IsDigit ch then Some() else None

let (|Whitespace|_|) ch = if Char.IsWhiteSpace ch then Some() else None

let gatherToken isType parse (expr : string) =
  let mutable i = 0
  while i < expr.Length && isType expr.[i] do i <- i + 1
  let next = expr.Substring(i)
  let value = expr.Substring(0, i) |> parse
  (value, next)

let parseOp = function '*' -> Times | '+' -> Plus | _ -> failwith "Invalid op"

let scan (expr : string) =
  let rec scanInt tokens (expr : string) =
    if String.IsNullOrWhiteSpace expr then tokens
    else
      let nextChar = expr.[0]
      match nextChar with
      | Digit ->
        let (num, next) = gatherToken Char.IsDigit Int32.Parse expr
        scanInt (Num num :: tokens) next
      | '+' | '*' -> scanInt (Op (parseOp nextChar) :: tokens) (expr.Substring(1))
      | '(' -> scanInt (OpenParen :: tokens) (expr.Substring(1))
      | ')' -> scanInt (CloseParen :: tokens) (expr.Substring(1))
      | Whitespace ->
        let (_, next) = gatherToken Char.IsWhiteSpace id expr
        scanInt tokens next
      | _ -> failwith "Invalid expression"
  scanInt [] expr |> List.rev

let getOp supported = function
  | Op op :: tail when List.contains op supported  -> Some(op, tail)
  | _ -> None

let rec literal parseExpr tokens =
  match tokens with
  | Num num :: tail -> (Literal num, tail)
  | OpenParen :: tail ->
    let (expr, tokens) = parseExpr tail
    match tokens with
    | CloseParen :: tail -> (expr, tail)
    | _ -> failwith "Expected closing parenthesis after grouped expression"
  | _ -> failwith "Invalid expression"

let rec operation getOp getExpr tokens =
  let (left, tokens) = getExpr tokens
  match getOp tokens with
  | Some(op, tail) ->
    let (right, tokens) = operation getOp getExpr tail
    (Binary (op, left, right), tokens)
  | _ -> (left, tokens)

let parseSamePrecedence tokens =
  let reverse ls =
    ls
    |> List.map (function OpenParen -> CloseParen | CloseParen -> OpenParen | t -> t)
    |> List.rev
  let rec parse tokens =
    operation (getOp [Plus; Times]) (literal parse) tokens
  parse (reverse tokens) |> fst

let parseOppositePrecedence tokens =
  let rec parse tokens =
    let term = operation (getOp [Plus]) (literal parse)
    let factor = operation (getOp [Times]) term
    factor tokens
  parse tokens |> fst

let rec evaluate expr =
  match expr with
  | Binary (op, left, right) ->
    let left = evaluate left
    let right = evaluate right
    match op with
    | Times -> left * right
    | Plus -> left + right
  | Literal n -> int64 n

[<EntryPoint>]
let main _ =
  let data = loadData()
  let samePrecedence = data |> Array.map (scan >> parseSamePrecedence >> evaluate)
  samePrecedence
  |> Array.sum
  |> printfn "The sum for same is: %d"

  let oppositePrecedence = data |> Array.map (scan >> parseOppositePrecedence >> evaluate)
  oppositePrecedence
  |> Array.sum
  |> printfn "The sum for opposite is: %d"
  0