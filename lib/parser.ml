open Lexer
open Tokens
open Printf

let format_tok (tok : Tokens.t) = 
  match tok with
  | Num i -> sprintf "NUM(%i)" i
  | Var s -> sprintf "VAR(%s)" s
  | MULT -> "MULT"
  | DIV -> "DIV"
  | PLUS -> "PLUS"
  | SUB -> "SUB"
  | EQ -> "EQ"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON"
  | AND -> "AND"
  | OR -> "OR"
  | MATCH -> "MATCH"
  | WITH -> "WITH"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | LEMMA -> "LEMMA"
  | FORALL -> "FORALL"
  | COMMA -> "COMMA"
  | PERIOD -> "PERIOD"
  | DEFINITION -> "DEFINITION"
  | EXISTS -> "EXISTS"
  | NAT -> "NAT"
  | REWRITE -> "REWRITE"
  | APPLY -> "APPLY"
  | INDUCTION -> "INDUCTION"
  | DESTRUCT -> "DESTRUCT"
  | SPLIT -> "SPLIT"
  | LEFT -> "LEFT"
  | RIGHT -> "RIGHT"
  | REFLEXIVITY -> "REFLEXIVITY"
  | PLACEHOLDER -> "PLACEHOLDER";;

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
  | Div

and bound_var = 
  | Bound_Var of string

and quantifier = 
  | Existential of bound_var list
  | Universal of bound_var list

and lemma = 
  | Lemma of string * comp;;

let dummy_pos : position = { line_num = -1; bol_off = -1; offset = -1};;

let dummy_tok : token = (PLACEHOLDER, dummy_pos);;

let parse_op (token : token) : op =
  let (tok, _) = token in
    match tok with
    | MULT -> Mult
    | DIV -> Div
    | PLUS -> Add
    | SUB -> Sub
    | _ -> Parsing_error ("Wrong op!", token) |> raise;;

let rec parse_binop (curr_expr : expr) (tokens : token list) : expr =
  match tokens with
  | op :: (Num y, _) :: ls -> 
      ls |> parse_binop (Binop ((parse_op op), curr_expr, (Peano (num_to_peano y))))
  | [] -> curr_expr
  | tok -> Parsing_error ("Malformed binop", List.hd tok) |> raise;;
 
let parse_expr (tokens : token list) : expr = 
  let parse_first_token tok toks = 
    match tok with
    | (Num y, _) -> let n1 = Peano (num_to_peano y) in parse_binop n1 toks
    | _ -> Parsing_error ("Binary operation must start with a number", tok) |> raise in

  match tokens with
  | hd :: toks -> parse_first_token hd toks
  | [] -> Parsing_error ("Expecting expression, got nothing!?", dummy_tok) |> raise;;

let is_after_eq token_index tokens : bool =
  let eq_index = List.find_index (fun (t, _) -> t = EQ) tokens in
    token_index > eq_index;;

let remove_last (ls, expr2) = 
  match (List.rev ls) with
  | [] -> ([], expr2)
  | [_] -> ([], expr2)
  | _ :: tl -> (List.rev tl, expr2);;

let parse_comp (tokens : token list) : comp = 
  let (expr1, expr2) =
    tokens
    |> List.mapi (fun i x -> (i, x))
    |> List.partition_map (fun (i, x) -> if is_after_eq (Some i) tokens then Right x else Left x) 
    |> remove_last in
      Eq (parse_expr expr1, parse_expr expr2);;

let parse_var (token : token) : bound_var =
  let (tok, _) = token in
  match tok with
  | Var x -> Bound_Var x
  | _ -> Parsing_error ("Expecting a var", token) |> raise;;

let rec parse_vars (tokens : token list) : bound_var list = 
  match tokens with
  | (COMMA, _) :: _ -> []
  | hd :: tl -> parse_var hd :: parse_vars tl
  | [] -> Parsing_error ("Nothing here!", dummy_tok) |> raise;;

let parse_bounds (tokens : token list) : bound_var list = 
  let parse_first_token tok = 
    match tok with
    | (Var _, _) -> parse_vars tokens 
    | (COMMA, _) -> Parsing_error ("Expecting some vars", tok) |> raise
    | _ -> Parsing_error ("Comma or Var is not here", tok) |> raise in
  
  match tokens with
  | hd :: _ ->  parse_first_token hd 
  | [] -> Parsing_error ("Nothing here!", dummy_tok) |> raise;;

let parse_quantifier (tokens : token list) : quantifier =
  let parse_first_token tok toks = 
  match tok with
  | (EXISTS, _) -> Existential (parse_bounds toks)
  | (FORALL, _) -> Universal (parse_bounds toks)
  | _ -> Parsing_error("Expecting Exists or Forall", tok) |> raise in

  match tokens with 
  | hd :: ls -> parse_first_token hd ls
  | [] -> Parsing_error ("Nothing here!", dummy_tok) |> raise

let parse_token (tok : token) exp =
  let (t, _) = tok in
  if t = exp then 
    ()
  else 
    let str = format_tok exp in 
      Parsing_error (Printf.sprintf "expecting %s" str, tok) |> raise;;

let parse_name tok =
  match tok with
  | (Var str, _) -> str
  | _ -> Parsing_error ("Not a Var!", tok) |> raise;;

let parse_lemma (tokens : token list) : lemma =  
  match tokens with
  | lemma :: nm :: cln :: ls -> 
      let nm = parse_name nm in 
        let _ = parse_token lemma LEMMA in
          let _ = parse_token cln COLON in
            Lemma (nm, parse_comp ls)
  | _ :: _ -> Parsing_error ("Too short to be a lemma", dummy_tok) |> raise
  | [] -> Parsing_error("You put nothing you loser", dummy_tok) |> raise;;

let parse_valid (tokens : token list) : lemma =
  match (List.rev tokens) with
  | (PERIOD, _) :: ls -> List.rev ls |> parse_lemma
  | _ -> Parsing_error("Must end command with PERIOD", dummy_tok) |> raise;;
