open STP.Lexer

let rec repl () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | str -> 
    let tokens = tokenize str 0 [] in
                print_tokens tokens;
      repl ();;

let () = repl ();;
