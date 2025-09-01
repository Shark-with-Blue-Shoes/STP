exception Lexing_error of string;;

open Printf
open Tokens

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf;;

let pos = ref 0;;

let next_pos () = 
  pos := !pos+1;;

let reset_pos () = 
  pos := 0;;

let rec tokenize (txt : string) (tokens : Tokens.t list) : Tokens.t list =
  
  let at_eof txt : bool = !pos >= String.length txt in
  (*This tokenizes numbers, it is initiated when the lexer finds a num*)
  let rec tokenize_num (chars : char list) =
    if at_eof txt |> not then begin
      let char = txt.[!pos] in
      (*If the character is a digit, then add it to the list and increment position*)
      if is_digit char then begin
        next_pos ();
        tokenize_num (char :: chars) end
      (*If there is a character right after a number, like 45b, raise an error*)
      else if is_alpha char then 
        raise (Lexing_error "Can't end number with letter, add a space or something")
      (*If the character is neither a digit or alpha, go back to the normal tokenizer, this'll probably change*)
      else 
        let final_num = List.rev chars |> string_of_chars |> int_of_string in
          let token = Num final_num in
          tokenize txt (token :: tokens) end 
    else begin
      let final_num = List.rev chars |> string_of_chars |> int_of_string in
        let token = Num final_num in
            (EOF :: token :: tokens) |> List.rev end
          in

  (*let rec tokenize_word (chars : char list) pos = 
    if at_eof pos txt |> not then begin
    else begin
      let final_num = List.rev chars |> string_of_chars |> int_of_string in
        let token = Num final_num in
            (EOF :: token :: tokens) |> List.rev end
          in*)
  if at_eof txt |> not then begin
    try
      let char = txt.[!pos] in
        if is_digit char then begin
          next_pos ();
          tokenize_num [char] end
        else begin
        next_pos ();
        match char with
        | '+' -> tokenize txt (PLUS :: tokens)
        | '/' -> tokenize txt (DIV :: tokens)
        | '*' -> tokenize txt (MULT :: tokens)
        | '-' -> tokenize txt (SUB :: tokens)
        | ' ' -> tokenize txt tokens
        | _ -> raise (Lexing_error "Not a symbol dum dum") end
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

let rec print_tokens (tokens : Tokens.t list) : unit =
  let print_token (tok : Tokens.t) : unit = 
    match tok with
    | Num i -> printf "NUM(%i)\n" i
    | Var s -> printf "VAR(%s)\n" s
    | MULT -> printf "MULT\n"
    | DIV -> printf "DIV\n"
    | PLUS -> printf "PLUS\n"
    | SUB -> printf "SUB\n"
    | EOF -> printf "EOF\n" in

  match tokens with 
  | tok :: ls -> print_token tok; print_tokens ls
  | [] -> ();;
