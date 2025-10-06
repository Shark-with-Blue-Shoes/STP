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
    sprintf "%s at %s" (format_tok tok) (format_pos pos);;

let error_of_token (err: string) (token : token) : string =
  sprintf "%s, got %s" err (format_token token);;

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

class parsing (tokens : token list) = object (self)

  val mutable toks = tokens

  method shift () =
   match toks with
   | [] -> Parsing_error "No more" |> raise
   | _ :: ls -> toks <- ls

  method shift_n num =
    let rec loop num =
      if num > 0 then begin
        self#shift (); loop (num-1) end
      else () in loop num
  
  method parse_expr : expr = 
    
    let rec parse_binop (start : expr) : expr =
      
        let match_op op = 
          match op with
          | MULT -> Mult
          | DIV -> Div
          | PLUS -> Add
          | SUB -> Sub
           in

      match toks with
      | (((MULT | DIV | PLUS | SUB) as op), _) :: (Tokens.Num y, _) :: _ -> 
          self#shift_n 2; Binop(match_op op, start, Num y) |> parse_binop
      | _ -> start
       in
    
    match toks with
    | [] -> Parsing_error "Where the helly are the tokens" |> raise
    | (Tokens.Num x, _) :: _ -> self#shift (); parse_binop (Num x) 
    | _ -> Parsing_error "Anomalous op" |> raise

  method parse_comp : comp = 
    let expr1 = self#parse_expr in 
    match toks with
    | (EQ, _) :: _ -> self#shift (); let expr2 = self#parse_expr in Eq (expr1, expr2)
    | [] -> Parsing_error "expected an eq sign here, got nothing" |> raise
    | _ -> Parsing_error (List.hd toks |> error_of_token "expected an eq sign here") |> raise

end
