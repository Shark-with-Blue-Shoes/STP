
type position = {
	mutable line_num : int; (*The line number*)
	mutable bol_off : int; (*The offset between the cursor and the start of the line*)
	mutable offset : int; (*The offset between the cursor and the start of the file*)
};;

type token = {
  t: Tokens.t;
  pos: position
}

let rec lexedls_to_toksls lexedls =
  match lexedls with
  | hd :: ls -> hd.t :: lexedls_to_toksls ls
  | [] -> [];;

exception Lexing_error of string * token list * position;;

open Tokens
open Printf

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf;;


let string_to_tok str : Tokens.t = 
  match str with
  | "match" -> MATCH
  | "with" -> WITH
  | "if" -> IF
  | "else" -> ELSE
  | "true" -> TRUE
  | "false" -> FALSE
  | "lemma" -> LEMMA
  | "forall" -> FORALL
  | "nat" -> NAT
  | "definition" -> DEFINITION
  | _ -> Var str;;

class position_tracker txt =
  object (self)
    
    val pos : position = {line_num = 1; bol_off = 0; offset = 0}
    
    method shiftr () =
      pos.offset <- pos.offset + 1;
      pos.bol_off <- pos.bol_off + 1
   
    method shiftl () =
      pos.offset <- pos.offset - 1;
      pos.bol_off <- pos.bol_off - 1

    method reset_bol_off () =
      pos.bol_off <- 0;
    
    method new_line () =
      pos.line_num <- pos.line_num + 1;
      self#reset_bol_off ();
      self#shiftr ()

    method current_off = pos.offset
    method current_line = pos.line_num
    method current_bol_off = pos.bol_off

    method get_pos = pos

    method at_eof () : bool = pos.offset >= String.length txt
  end

class lexer (str : string) = object (self)
  (*The string being lexed*)
  val txt : string = str
 
  val pos_tracker = new position_tracker str 

(*Tokenizes txt*)
method tokenize (tokens : token list) : token list =

  let tokenize_next toks = pos_tracker#shiftr (); self#tokenize toks in

  let tokenize_nline toks = pos_tracker#new_line (); self#tokenize toks in

  (*This tokenizes numbers, it is initiated when the lexer finds a num*)
  let rec tokenize_num (chars : char list) =
    pos_tracker#shiftr ();
    if pos_tracker#at_eof () |> not then begin
      let char = txt.[pos_tracker#current_off] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' ->  
          Lexing_error ("Can't end number with letter, add a space or something", tokens, pos_tracker#get_pos) |> raise;
      | '0' .. '9' -> chars @ [char] |> tokenize_num 
      | _ -> pos_tracker#shiftl ();
            let final_num = chars |> string_of_chars |> int_of_string in
            let token = Num final_num in token end
    else
      let final_num = chars |> string_of_chars |> int_of_string in
        let token = Num final_num in token
          in

  let rec tokenize_word (chars : char list) : Tokens.t = 
    pos_tracker#shiftr ();
    if pos_tracker#at_eof () |> not then begin
      let char = txt.[pos_tracker#current_off] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> chars @ [char] |> tokenize_word
      | _ -> pos_tracker#shiftl (); let token = chars |> string_of_chars |> string_to_tok in token end
    else 
      let token = chars |> string_of_chars |> string_to_tok in token
          in

  let rec skip_comment () : unit = 
    pos_tracker#shiftr ();
    if pos_tracker#at_eof () |> not then begin
      let char = txt.[pos_tracker#current_off] in
      match char with
      | '\\' -> ()
      | _ -> skip_comment ()
    end
    else 
    Lexing_error ("Forgot to close comment", tokens, pos_tracker#get_pos) |> raise in

  if pos_tracker#at_eof () |> not then begin
    let char = txt.[pos_tracker#current_off] in
    match char with
    | ' ' | '\t' | '\r' -> tokenize_next tokens
    | '\n' -> tokenize_nline tokens
    | '\\' -> skip_comment (); tokenize_next tokens
    | _ -> let p = pos_tracker#get_pos in
           let t = match char with
                       | 'a' .. 'z' | 'A' .. 'Z' ->  tokenize_word [char]
                       | '0' .. '9' -> tokenize_num [char]
                       | '+' -> PLUS
                       | '/' -> DIV
                       | '*' -> MULT
                       | '-' -> SUB 
                       | '=' -> EQ
                       | '(' -> LPAREN
                       | ')' -> RPAREN
                       | '{' -> LBRACE
                       | '}' -> RBRACE
                       | '[' -> LBRACK
                       | ']' -> RBRACK
                       | ';' -> SEMICOLON
                       | ':' -> COLON
                       | '&' -> AND
                       | '|' -> OR
                       | ',' -> COMMA
                       | '.' -> PERIOD
                       | t -> Lexing_error (sprintf "%c does not match any known char" t, tokens, pos_tracker#get_pos) |> raise in
                            let token = {t = t; pos = p} in
                            tokenize_next (tokens @ [token]) end
  else
    let p = pos_tracker#get_pos in
    let token = {t = EOF; pos = p} in
    tokens @ [token]
end

