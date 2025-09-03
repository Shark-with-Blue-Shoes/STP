exception Parsing_error of string;;

open Printf

type expr = 
  | Num of int
  | Binop of op * expr * expr

and op = 
  | Add
  | Sub
  | Mult
  | Div;;

let print_expr (expr : expr) =
  let format_op op =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "//" in

  let rec format_expr expr = 
    match expr with 
    | Num n -> sprintf "Num(%d)" n
    | Binop (op, expr1, expr2) -> 
        sprintf "Binop(%s, %s, %s)" (format_op op) (format_expr expr1) (format_expr expr2) in
  let str = format_expr expr in
  printf "%s\n" str;;

let parse_op (token : Tokens.t) : op =
  let open Tokens in
    match token with
    | MULT -> Mult
    | DIV -> Div
    | PLUS -> Add
    | SUB -> Sub
    | _ -> Parsing_error "Wrong op!" |> raise;;

let parse_expr (tokens : Tokens.t list) : expr = 
  let rec expr_loop (start : expr) (tokens : Tokens.t list) : expr =
    Lexer.print_tokens tokens;
    match tokens with
    | op :: Tokens.Num y :: ls -> expr_loop (Binop ((parse_op op), start, (Num y))) ls
    | [EOF] -> start
    | _ -> Parsing_error "Anomalous binop" |> raise
    in
  match tokens with
  | [] -> Parsing_error "Where the helly are the tokens" |> raise
  | hd :: tl -> (match hd with
                | Tokens.Num x -> let n1 = Num x in expr_loop n1 tl
                | _ -> Parsing_error "Anomalous op" |> raise);;

