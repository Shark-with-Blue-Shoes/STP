type cursor = {
  mutable line_num : int;
  mutable bol_off : int;
  mutable offset : int;
}

type position = { line_num : int; bol_off : int; offset : int; }

val dummy : position

val curs_to_pos : cursor -> position

val pos_to_curs : position -> cursor

type token = Tokens.t * position

exception Lexing_error of string * Tokens.t list * cursor

val string_of_chars : char list -> string

val string_to_tac : string -> Tokens.t

val string_to_tok : string -> Tokens.t

class cursor_tracker :

  string ->
  object
    val curs : cursor
    method at_eof : unit -> bool
    method current_bol_off : int
    method current_line : int
    method current_off : int
    method get_curs : cursor
    method new_line : unit -> unit
    method reset_bol_off : unit -> unit
    method shiftl : unit -> unit
    method shiftr : unit -> unit
  end

class lexer :
  string ->
  object
    val cursor : cursor_tracker
    val txt : string
    method tokenize : token list -> token list
  end
