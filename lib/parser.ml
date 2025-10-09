open Lexer
open Tokens
open Printf

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

and comp = 
  | Eq of expr * expr

and lemma = string * comp

and tactic = 
  | Reflexivity
  | Simpl;;

class parse_lemma (tokens : token list) = object (self)

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
          | PLUS -> Add
          | SUB -> Sub
           in

      match toks with
      | (((MULT | PLUS | SUB) as op), _) :: (Tokens.Num y, _) :: _ -> 
          self#shift_n 2; Binop(match_op op, start, Num y) |> parse_binop
      | ((MULT | PLUS | SUB), _) :: _ -> 
          Parsing_error "I expect a number after an operator" |> raise
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
    | [] -> Parsing_error "Expected an eq sign here, got nothing" |> raise
    | hd :: _ -> Parsing_error (error_of_token "Expected an eq sign here" hd) |> raise
  
  method end_of_lemma : unit = 
    match toks with
    | hd :: _ -> Parsing_error (error_of_token "Did not end lemma correctly" hd) |> raise
    | [] -> ()

  method parse_lemma : lemma = 
    match toks with
    | (LEMMA, _) :: (Var str, _) :: (COLON, _) :: _ -> self#shift_n 3; let lem = (str, self#parse_comp) in self#end_of_lemma; lem
    | (LEMMA, _) ::  _ ->  Parsing_error "Where's the name" |> raise
    | [] -> Parsing_error "Expect lemma, got nothing" |> raise
    | hd :: _ -> Parsing_error (error_of_token "Expected lemma" hd) |> raise

end

class parse_tactic (tokens : token list) = object (self)

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
  
  method parse_tactic : tactic = 
    match toks with
    | [(REFLEXIVITY, _)] -> Reflexivity
    | [(SIMPL, _)] -> Simpl
    | _ -> Parsing_error "I hate you!" |> raise
end
