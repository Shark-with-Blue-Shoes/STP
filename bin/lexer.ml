exception Lexing_error of string;;

open Printf
open Tokens

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let at_eof pos txt : bool = pos >= String.length txt;;

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf;;

let rec tokenize (txt : string) (pos : int) (tokens : Tokens.t list) : Tokens.t list =

  let rec tokenize_num (chars : char list) pos =
    if at_eof pos txt |> not then begin
      if is_digit txt.[pos] then 
        tokenize_num (txt.[pos] :: chars) (pos+1)
      else if is_alpha txt.[pos] then 
        raise (Lexing_error "Can't end number with letter, add a space or something")
      else 
        let final_num = List.rev chars |> string_of_chars |> int_of_string in
          let token = Num final_num in
          tokenize txt (pos) (token :: tokens) end 
    else begin
      let final_num = List.rev chars |> string_of_chars |> int_of_string in
        let token = Num final_num in
            (EOF :: token :: tokens) |> List.rev end
          in

  if at_eof pos txt |> not then begin
    try
      let char = txt.[pos] in
        if is_digit char then
          tokenize_num [char] (pos+1)
        else
        match char with
        | '+' -> tokenize txt (pos+1) (PLUS :: tokens)
        | '/' -> tokenize txt (pos+1) (DIV :: tokens)
        | '*' -> tokenize txt (pos+1) (MULT :: tokens)
        | '-' -> tokenize txt (pos+1) (SUB :: tokens)
        | ' ' -> tokenize txt (pos+1) tokens
        | _ -> raise (Lexing_error "Not a symbol dum dum")
    with 
    | Lexing_error err -> 
        printf "LEXING ERROR: %s\nat offset: %i\n\n\nPrinting retrieved tokens...\n" err (pos+1); 
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
    | MULT -> printf "MULT\n"
    | DIV -> printf "DIV\n"
    | PLUS -> printf "PLUS\n"
    | SUB -> printf "SUB\n"
    | EOF -> printf "EOF\n" in

  match tokens with 
  | tok :: ls -> print_token tok; print_tokens ls
  | [] -> ();;
