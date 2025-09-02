exception Lexing_error of string;;

open Printf
open Tokens

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf;;

let pos = ref 0;;

let next_pos () = 
  pos := !pos+1;;

let back_pos () = 
  pos := !pos-1;;

let reset_pos () = 
  pos := 0;;


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

let rec tokenize (txt : string) (tokens : Tokens.t list) : Tokens.t list =

  let tokenize_next cur_tokens = next_pos (); tokenize txt cur_tokens in

  let at_eof () : bool = !pos >= String.length txt in
  (*This tokenizes numbers, it is initiated when the lexer finds a num*)
  let rec tokenize_num (chars : char list) =
    next_pos ();
    if at_eof () |> not then begin
      let char = txt.[!pos] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' -> Lexing_error "Can't end number with letter, add a space or something" |> raise 
      | '0' .. '9' -> tokenize_num (char :: chars)
      | _ -> back_pos ();
            let final_num = List.rev chars |> string_of_chars |> int_of_string in
            let token = Num final_num in token end
    else
      let final_num = List.rev chars |> string_of_chars |> int_of_string in
        let token = Num final_num in token
          in

  let rec tokenize_word (chars : char list) : Tokens.t = 
    next_pos ();
    if at_eof () |> not then begin
      let char = txt.[!pos] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> tokenize_word (char :: chars)
      | _ -> back_pos (); let token = List.rev chars |> string_of_chars |> string_to_tok in token end
    else 
      let token = List.rev chars |> string_of_chars |> string_to_tok in token
          in

  let rec skip_comment () : unit = 
    next_pos ();
    if at_eof () |> not then begin
      let char = txt.[!pos] in
      match char with
      | '\\' -> ()
      | _ -> skip_comment ()
    end
    else 
    Lexing_error "Forgot to close comment" |> raise in

  if at_eof () |> not then begin
    try
      let char = txt.[!pos] in
      match char with
      | ' ' | '\t' | '\n' | '\r' -> tokenize_next tokens
      | '\\' -> skip_comment (); tokenize_next tokens
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
                                  tokenize_next (token :: tokens)
    with 
    | Lexing_error err -> 
        printf "LEXING ERROR: %s\nat offset: %i\n\n\nPrinting retrieved tokens...\n" err !pos; 
        tokens |> List.rev 
    | err -> 
        Printexc.to_string err |> printf "ANOMALY: %s\n\n\nPrinting retrieved tokens...\n\n";
        tokens |> List.rev end
  else 
    (EOF :: tokens) |> List.rev 
;;

let rec print_tokens (tokens : t list) : unit =
  let print_token (tok : t) : unit = 
    match tok with
    | Num i -> printf "NUM(%i)\n" i
    | Var s -> printf "VAR(%s)\n" s
    | MULT -> printf "MULT\n"
    | DIV -> printf "DIV\n"
    | PLUS -> printf "PLUS\n"
    | SUB -> printf "SUB\n"
    | EQ -> printf "EQ\n"
    | LPAREN -> printf "LPAREN\n"
    | RPAREN -> printf "RPAREN\n"
    | LBRACE -> printf "LBRACE\n"
    | RBRACE -> printf "RBRACE\n"
    | LBRACK -> printf "LBRACK\n"
    | RBRACK -> printf "RBRACK\n"
    | SEMICOLON -> printf "SEMICOLON\n"
    | COLON -> printf "COLON\n"
    | AND -> printf "AND\n"
    | OR -> printf "OR\n"
    | MATCH -> printf "MATCH\n"
    | WITH -> printf "WITH\n"
    | IF -> printf "IF\n"
    | ELSE -> printf "ELSE\n"
    | TRUE -> printf "TRUE"
    | FALSE -> printf "FALSE\n"
    | LEMMA -> printf "LEMMA\n"
    | FORALL -> printf "FORALL\n"
    | COMMA -> printf "COMMA\n"
    | PERIOD -> printf "PERIOD\n"
    | DEFINITION -> printf "DEFINITION\n"
    | NAT -> printf "NAT\n"
    | EOF -> printf "EOF\n"
  in
  match tokens with
  | [] -> ()
  | hd :: tl ->
      print_token hd;
      print_tokens tl
