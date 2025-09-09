open Lexer

exception Parsing_error of string * token;;

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

and prop =
  | Eq of expr * expr

and op = 
  | Add
  | Sub
  | Mult
  | Div;;

let dummy_curs : cursor = { line_num = -1; bol_off = -1; offset = -1};;

let parse_op (token : token) : op =
  let open Tokens in
  let (tok, _) = token in
    match tok with
    | MULT -> Mult
    | DIV -> Div
    | PLUS -> Add
    | SUB -> Sub
    | EOF -> Lexing_error ("EOF token in the middle of token list -> notify maintainers of problem", [token], dummy_curs) |> raise
    | _ -> Parsing_error ("Wrong op!", token) |> raise;;

let rec parse_binop (curr_expr : expr) (tokens : token list) : expr =
  match tokens with
  | op :: (Tokens.Num y, _) :: ls -> 
      ls |> parse_binop (Binop ((parse_op op), curr_expr, (Peano (num_to_peano y))))
  | [(Tokens.EOF, _)] -> curr_expr
  | [] -> Lexing_error ("No tokens to parse in binop -> notify maintainers of problem", [], dummy_curs) |> raise
  | tok -> Parsing_error ("Malformed binop", List.hd tok) |> raise;;
 
let parse_expr (tokens : token list) : expr = 
  match tokens with
  | (Tokens.Num y, _) :: ls -> let n1 = Peano (num_to_peano y) in parse_binop n1 ls
  | [] -> Lexing_error ("No tokens to parse in expression -> notify maintainers of problem", [], dummy_curs) |> raise
  | tok -> Parsing_error ("Binary operation must start with a number", List.hd tok) |> raise;;

