open Lexer
open Printf

let rec repl () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | str -> 
    printf "input is %i long\n" (String.length str);
    let tokens = tokenize str 0 [] in
                print_tokens tokens;
      repl ();;

let () = repl ();;
