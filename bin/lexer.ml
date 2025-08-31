exception Lexing_error of string;;

open Printf
open Tokens

let rec tokenize (txt : string) (pos : int) (tokens : Tokens.t list) : Tokens.t list =
  if pos < String.length txt then begin
    let char = txt.[pos] in
      match char with
      | '+' -> tokenize txt (pos+1) (PLUS :: tokens)
      | '/' -> tokenize txt (pos+1) (DIV :: tokens)
      | '*' -> tokenize txt (pos+1) (MULT :: tokens)
      | '-' -> tokenize txt (pos+1) (SUB :: tokens)
      | ' ' -> tokenize txt (pos+1) tokens
      | _ -> raise (Lexing_error "screw you") end
  else 
    (EOF :: tokens) |> List.rev 
;;

let rec print_tokens (tokens : Tokens.t list) : unit =
  let sort_token (tok : Tokens.t) : unit = 
    match tok with
    | Num i -> printf "NUM(%i)\n" i
    | MULT -> printf "MULT\n"
    | DIV -> printf "DIV\n"
    | PLUS -> printf "PLUS\n"
    | SUB -> printf "SUB\n"
    | EOF -> printf "EOF\n" in

  match tokens with 
  | tok :: ls -> sort_token tok; print_tokens ls
  | [] -> ();;
