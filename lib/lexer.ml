exception Lexing_error of string;;

open Printf
open Tokens
open Printer

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

type pos = { mutable offset: int; mutable line: int}

class position_tracker txt =
  object
    
    val mutable pos = 0
    
    method shiftr () =
      pos <- pos + 1
   
    method shiftl () =
      pos <- pos - 1

    method current_pos = pos

    method at_eof () : bool = pos >= String.length txt
  end

class lexer (str : string) = object (self)
  (*The string being lexed*)
  val txt : string = str
 
  val pos_tracker = new position_tracker str 
  val mutable tokens : Tokens.t list = []
  
  method get_tokens = tokens

  (*Tokenizes txt*)
  method tokenize : unit =

  let tokenize_next () = pos_tracker#shiftr (); self#tokenize in

  (*This tokenizes numbers, it is initiated when the lexer finds a num*)
  let rec tokenize_num (chars : char list) =
    pos_tracker#shiftr ();
    if pos_tracker#at_eof () |> not then begin
      let char = txt.[pos_tracker#current_pos] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' ->  
          Lexing_error "Can't end number with letter, add a space or something" |> raise;
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
      let char = txt.[pos_tracker#current_pos] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> chars @ [char] |> tokenize_word
      | _ -> pos_tracker#shiftl (); let token = chars |> string_of_chars |> string_to_tok in token end
    else 
      let token = chars |> string_of_chars |> string_to_tok in token
          in

  let rec skip_comment () : unit = 
    pos_tracker#shiftr ();
    if pos_tracker#at_eof () |> not then begin
      let char = txt.[pos_tracker#current_pos] in
      match char with
      | '\\' -> ()
      | _ -> skip_comment ()
    end
    else 
    Lexing_error "Forgot to close comment" |> raise in

  if pos_tracker#at_eof () |> not then begin
    try
      let char = txt.[pos_tracker#current_pos] in
      match char with
      | ' ' | '\t' | '\n' | '\r' -> tokenize_next ()
      | '\\' -> skip_comment (); tokenize_next ()
      | _ -> let token = match char with
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
                         | _ -> Lexing_error "Does not match any known char" |> raise in
                                tokens <- tokens @ [token];
                                  tokenize_next ()
    with 
    | Lexing_error err -> print_tokens tokens;
        printf "\n\n\nPrinted Retrieved Tokens\n\nLEXING ERROR: %s\nat offset: %i\n" err pos_tracker#current_pos; 
    | err -> 
        Printexc.to_string err |> printf "ANOMALY: %s\n"; end
  else
    tokens <- tokens @ [EOF]
end

