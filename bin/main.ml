open STP.Lexer

let rec repl () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | str -> let lex = new lexer str in
            let _ = lex#tokenize in
                lex#pretty_print;
      repl ();;

let () = repl ();;
