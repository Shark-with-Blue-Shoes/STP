val format_tok : Tokens.t -> string

exception Parsing_error of string * Lexer.token

type peano = O | S of peano

val num_to_peano : int -> peano

type expr = Peano of peano | Binop of op * expr * expr

and comp = Eq of expr * expr

and op = Add | Sub | Mult | Div

and bound_var = Bound_Var of string

and quantifier = Existential of bound_var list | Universal of bound_var list

and lemma = string * comp

val dummy_pos : Lexer.position

val dummy_tok : Lexer.token

val parse_op : Lexer.token -> op

val parse_binop : expr -> Lexer.token list -> expr

val parse_expr : Lexer.token list -> expr

val remove_at : int -> 'a list -> 'a list

val parse_comp : Lexer.token list -> comp

val parse_var : Lexer.token -> bound_var

val parse_vars : Lexer.token list -> bound_var list

val parse_bounds : Lexer.token list -> bound_var list

val parse_quantifier : Lexer.token list -> quantifier

val parse_token : Lexer.token -> Tokens.t -> unit

val parse_name : Lexer.token -> string

val parse_lemma : Lexer.token list -> lemma

val parse_input : Lexer.token list -> (Lexer.token list -> 'a) -> 'a 

type tactic = 
  | Reflexivity
  | Rewrite of string;;

val parse_tactic : Lexer.token list -> tactic
