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

let findToMatchAndYourTree (env: Map<string, Expr>) expr =
    let rec pathToHuman currentPath expr =
        match expr with
        | Var name ->
            if name = "humn" then
                Some(currentPath |> List.rev)
            else
                pathToHuman currentPath env[name]
        | Literal _ -> None
        | Binary (_, left, right) ->
            let left = pathToHuman (true :: currentPath) left
            let right = pathToHuman (false :: currentPath) right

            match left, right with
            | Some path, None
            | None, Some path -> Some path
            | _ -> None

    match expr with
    | Binary (_, e1, e2) ->
        let path1 = e1 |> pathToHuman []
        let path2 = e2 |> pathToHuman []

        match path1, path2 with
        | Some (path), None -> ((e1, path), e2)
        | None, Some (path) -> ((e2, path), e1)
        | _ -> failwith "Invalid expr"
    | _ -> failwith "Invalid expr"

let rec exprToString (env: Map<string, Expr>) expr =
    match expr with
    | Literal value -> sprintf "%d" value
    | Var name -> exprToString env env[name]
    | Binary (op, left, right) ->
        let op =
            match op with
            | Multiply -> "*"
            | Divide -> "/"
            | Plus -> "+"
            | Minus -> "-"

        sprintf "%s %s %s" (exprToString env left) op (exprToString env right)


let oppositeOperation =
    function
    | Multiply -> Divide
    | Divide -> Multiply
    | Plus -> Minus
    | Minus -> Plus

let rec rewriteExpressionToMatch env path currentExpr toRewrite =
    match toRewrite, path with
    | Binary (Divide, left, right), false :: rest ->
        let opposite =
            evaluate env (Binary(Divide, Literal 1L, (Binary(Divide, currentExpr, left))))

        rewriteExpressionToMatch env rest (Literal opposite) (right)
    | Binary (Minus, left, right), false :: rest ->
        let opposite =
            evaluate env (Binary(Multiply, Literal -1L, (Binary(Minus, currentExpr, left))))

        rewriteExpressionToMatch env rest (Literal opposite) (right)
    | Binary (op, left, right), isLeft :: rest ->
        let evaluated = evaluate env (if isLeft then right else left)
        let opposite = oppositeOperation op
        let currentExpr = Binary(opposite, currentExpr, Literal evaluated)
        rewriteExpressionToMatch env rest currentExpr (if isLeft then left else right)
    | Var name, path ->
        if name = "humn" then
            currentExpr
        else
            rewriteExpressionToMatch env path currentExpr (env[name])
    | expr, path -> failwithf "Invalid expression: %A %A" expr path

let loadData () =
    let text = System.IO.File.ReadAllLines("./input.txt")

    text |> Array.toList |> parseExpressions Map.empty

let data = loadData ()

evaluate data (Var "root") |> printfn "Part 1: %d"

let ((yours, pathToYours), toMatch) = findToMatchAndYourTree data data["root"]
let valueToMatch = evaluate data toMatch

rewriteExpressionToMatch data pathToYours toMatch yours
|> evaluate data
|> printfn "Part 2: %d"
