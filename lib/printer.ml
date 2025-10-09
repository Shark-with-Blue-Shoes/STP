open Printf
open Parser

let format_tok (tok : Tokens.t) = 
  match tok with
  | Num i -> sprintf "NUM(%i)" i
  | Var s -> sprintf "VAR(%s)" s
  | MULT -> "MULT"
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

let rec print_tokens toks : unit =
  match toks with
  | [] -> ()
  | hd :: tl ->
      format_tok hd |> printf "%s\n";
    print_tokens tl;;

let fop op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*";;

let rec fexpr expr = 
  match expr with 
  | Num n -> sprintf "%d" n
  | Binop (op, expr1, expr2) -> 
      sprintf "(%s %s %s)" (fexpr expr1) (fop op) (fexpr expr2);;

let fcomp comp = 
  match comp with
  | Eq (expr1, expr2) -> sprintf "%s = %s" (fexpr expr1) (fexpr expr2);;

let flemma (name, comp) = 
  sprintf "Lemma %s : %s" name (fcomp comp);;

let print (func : 'a -> string) (obj : 'a) =
  let str = func obj in
  printf "%s\n" str;;

