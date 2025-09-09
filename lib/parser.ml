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

and comp =
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
  | [] -> curr_expr
  | tok -> Parsing_error ("Malformed binop", List.hd tok) |> raise;;
 
let parse_expr (tokens : token list) : expr = 
  match tokens with
  | (Tokens.Num y, _) :: ls -> let n1 = Peano (num_to_peano y) in parse_binop n1 ls
  | [] -> Parsing_error ("No tokens to parse in expression", (EQ, curs_to_pos dummy_curs)) |> raise
  | tok -> Parsing_error ("Binary operation must start with a number", List.hd tok) |> raise;;

let is_after_eq token_index tokens : bool =
  let eq_index = List.find_index (fun (t, _) -> t = Tokens.EQ) tokens in
    token_index > eq_index;;

let remove_tails (lst1, lst2) =
  let rec remove_tail = function
    | [] -> []
    | [_] -> []
    | x :: xs -> x :: remove_tail xs
  in
  (remove_tail lst1, remove_tail lst2)

let parse_comp (tokens : token list) : comp = 
  let (expr1, expr2) =
    tokens
    |> List.mapi (fun i x -> (i, x))
    |> List.partition_map (fun (i, x) -> if is_after_eq (Some i) tokens then Right x else Left x) 
    |> remove_tails in
      Eq (parse_expr expr1, parse_expr expr2);;
