val format_tok : Tokens.t -> string

val format_pos : Lexer.position -> string

val format_token : Lexer.token -> string

val error_of_token : string -> Lexer.token -> string

exception Parsing_error of string

type expr = Num of int | Binop of op * expr * expr

and op = Add | Sub | Mult | Div

and comp = Eq of expr * expr

and lemma = string * comp

and tactic = Reflexivity

class parse_lemma :
  Lexer.token list ->
  object
    val mutable toks : Lexer.token list
    method end_of_lemma : unit
    method parse_comp : comp
    method parse_expr : expr
    method parse_lemma : lemma
    method shift : unit -> unit
    method shift_n : int -> unit
  end

class parse_tactic :
  Lexer.token list ->
  object
    val mutable toks : Lexer.token list
    method parse_tactic : tactic
    method shift : unit -> unit
    method shift_n : int -> unit
  end
