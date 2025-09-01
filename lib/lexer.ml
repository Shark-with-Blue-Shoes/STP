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
    else begin
      let final_num = List.rev chars |> string_of_chars |> int_of_string in
        let token = Num final_num in token end
          in

  let rec tokenize_word (chars : char list) : Tokens.t = 
    next_pos ();
    if at_eof () |> not then begin
      let char = txt.[!pos] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> tokenize_word (char :: chars)
      | ' ' | '\t' | '\n' | '\r' ->  
                                    let final_word = List.rev chars |> string_of_chars in
                                    let token = Var final_word in
                                    token
      | _ -> Lexing_error "What the helly is this character!" |> raise end
    else begin
      let final_word = List.rev chars |> string_of_chars in
        let token = Var final_word in token end
          in

  if at_eof () |> not then begin
    try
      let char = txt.[!pos] in
      match char with
      | ' ' | '\t' | '\n' | '\r' -> tokenize_next tokens
      | _ -> let token = match char with
                         | 'a' .. 'z' | 'A' .. 'Z' ->  tokenize_word [char]
                         | '0' .. '9' -> tokenize_num [char]
                         | '+' -> PLUS
                         | '/' -> DIV
                         | '*' -> MULT
                         | '-' -> SUB 
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
    | EOF -> printf "EOF\n"
  in
  match tokens with
  | [] -> ()
  | hd :: tl ->
      print_token hd;
      print_tokens tl
