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
  | EOF -> "EOF";;

let format_pos (pos : position) : string = 
  sprintf "line %d, offset %d" pos.line_num pos.bol_off;;

let format_token (token : token) : string = 
  let (tok, pos) = token in
    sprintf "%s: %s\n" (format_tok tok) (format_pos pos);;

let error_of_token (err: string) (token : token) : string =
  sprintf "%s at %s" err (format_token token);;

exception Parsing_error of string;;

type expr = 
  | Num of int
  | Binop of op * expr * expr

and op = 
  | Add
  | Sub
  | Mult
  | Div

and comp = 
  | Eq of expr * expr;;

let parse_op (token : token) : op =
  let (tok, _) = token in
    match tok with
    | MULT -> Mult
    | DIV -> Div
    | PLUS -> Add
    | SUB -> Sub
    | _ -> Parsing_error (error_of_token "Expecting a binop, got" token) |> raise;;


let rec parse_binop (start : expr) (tokens : token list) : expr =
  match tokens with
  | op :: (Tokens.Num y, _) :: ls -> parse_binop (Binop ((parse_op op), start, (Num y))) ls
  | [(EOF, _)] -> start
  | [] -> Parsing_error "Expecting an EOF, got nothing" |> raise
  | ls -> Parsing_error (List.hd ls |> error_of_token "Expecting an (op + num) or EOF, got") |> raise;;

let parse_expr (tokens : token list) : expr = 

  let parse_first_token hd tl = 
    match hd with
    | (Tokens.Num x, _) -> let n1 = Num x in parse_binop n1 tl
    | _ -> Parsing_error "Anomalous op" |> raise in

  match tokens with
  | [] -> Parsing_error "Where the helly are the tokens" |> raise
  | hd :: tl -> parse_first_token hd tl;;

(*and parse_comp (tokens : token list) : comp =
  let parse_first_token hd tl = 
    match hd with
    | (Tokens.Num x, _) -> let n1 = Num x in parse_binop n1 tl
    | _ -> Parsing_error "Anomalous comp" |> raise in*)
