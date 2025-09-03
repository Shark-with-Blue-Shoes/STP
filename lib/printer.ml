open Printf
open Parser

let rec print_tokens toks : unit =
  let print_token (tok : Tokens.t) : unit = 
    let str = 
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
    | NAT -> "NAT"
    | EOF -> "EOF" in printf "%s\n" str;
in
match toks with
| [] -> ()
| hd :: tl ->
    print_token hd;
    print_tokens tl;;

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

