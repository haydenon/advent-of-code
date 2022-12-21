open System

type Op =
    | Multiply
    | Divide
    | Plus
    | Minus


type Expr =
    | Binary of Op * Expr * Expr
    | Var of string
    | Literal of int64

let rec parseExpressions exprs (lines: string list) : Map<string, Expr> =
    // root: pppw + sjmn
    let parseExpression (expr: string) =
        let parts = expr.Trim().Split(" ")

        if parts |> Array.length = 1 then
            Literal(parts[0] |> Int64.Parse)
        else
            let left = Var parts[0]
            let right = Var parts[2]

            let op =
                match parts[1] with
                | "*" -> Multiply
                | "/" -> Divide
                | "+" -> Plus
                | "-" -> Minus
                | _ -> failwith "Invalid input"

            Binary(op, left, right)

    match lines with
    | [] -> exprs
    | line :: rest ->
        let parts = line.Split(":")

        let newExprs =
            exprs
            |> Map.add parts[0] (parseExpression parts[1])

        parseExpressions newExprs rest

let rec evaluate (env: Map<string, Expr>) expr =
    match expr with
    | Var name -> evaluate env env[name]
    | Literal value -> value
    | Binary (op, left, right) ->
        let left = evaluate env left
        let right = evaluate env right

        match op with
        | Multiply -> left * right
        | Divide -> left / right
        | Plus -> left + right
        | Minus -> left - right

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> Array.toList |> parseExpressions Map.empty

let data = loadData ()

evaluate data (Var "root") |> printfn "Part 1: %d"

