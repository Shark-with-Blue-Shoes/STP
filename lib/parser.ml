open Lexer

exception Parsing_error of string * token;;

(*For the really weird situations*)
exception Anomalous_error of string;;

type peano = 
  | O
  | S of peano;;

let rec num_to_peano (num : int) : peano = 
  match num with
  | 0 -> O
  | _ -> S (num_to_peano (num-1));;

type expr = 
  | Peano of peano
  | Binop of op * expr * expr

and op = 
  | Add
  | Sub
  | Mult
  | Div;;

let parse_op (token : token) : op =
  let open Tokens in
  let tok = token.t in
    match tok with
    | MULT -> Mult
    | DIV -> Div
    | PLUS -> Add
    | SUB -> Sub
    | EOF -> Parsing_error ("Ended abruptly", token) |> raise
    | _ -> Parsing_error ("Wrong op!", token) |> raise;;

let def_pos : position = { line_num = 0; bol_off = 0; offset = 0};;

let parse_expr (tokens : Lexer.token list) : expr = 
  
  let rec parse_binop (curr_expr : expr) (tokens : Lexer.token list) : expr =
    match tokens with
    | op :: {t = Tokens.Num y; _} :: ls -> 
        ls |> parse_binop (Binop ((parse_op op), curr_expr, (Peano (num_to_peano y))))
    | [{t = Tokens.EOF; _}] -> curr_expr
    | [] -> Anomalous_error "Where the helly is EOF?" |> raise
    | tok -> Parsing_error ("Malformed binop", List.hd tok) |> raise
    in

  match tokens with
  | {t = Tokens.Num y; _} :: ls -> let n1 = Peano (num_to_peano y) in parse_binop n1 ls
  | [] -> Anomalous_error "Where the helly are the tokens" |> raise
  | tok -> Parsing_error ("Binary operation must start with a number", List.hd tok) |> raise;;
