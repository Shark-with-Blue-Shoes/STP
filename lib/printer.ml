open Printf
open Tokens
open Parser
open Lexer

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
  | EOF -> "EOF"
  | REWRITE -> "REWRITE"
  | APPLY -> "APPLY"
  | INDUCTION -> "INDUCTION"
  | DESTRUCT -> "DESTRUCT"
  | SPLIT -> "SPLIT"
  | LEFT -> "LEFT"
  | RIGHT -> "RIGHT"
  | REFLEXIVITY -> "REFLEXIVITY";;


let format_pos (p: position) = 
  sprintf "line num: %d, offset: %d" p.line_num p.bol_off;;

let rec print_tokens toks : unit =
  let print_token (tok : token) : unit = 
    let (t, _) = tok in
    let tokstr = format_tok t in
    printf "%s\n" tokstr;
in
match toks with
| [] -> ()
| hd :: tl ->
    print_token hd;
    print_tokens tl;;

let format_op op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "//";;

let rec format_peano (p : peano) : string =
  match p with
  | S p -> sprintf "S (%s)" (format_peano p)
  | O -> "O";;

let rec format_expr expr = 
  match expr with 
  | Peano n -> format_peano n |> sprintf "Peano(%s)"
  | Binop (op, expr1, expr2) -> 
      sprintf "Binop(%s, %s, %s)" (format_op op) (format_expr expr1) (format_expr expr2);;

let print_expr (expr : expr) =
  let str = format_expr expr in
  printf "%s\n" str;;

let format_comp comp = 
  match comp with
  | Eq (expr1, expr2) -> sprintf "Eq(%s, %s)" (format_expr expr1) (format_expr expr2);;

let print_comp (comp : comp) =
  let str = format_comp comp in
  printf "%s\n" str;;
