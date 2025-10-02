open STP.Lexer
open STP.Parser
open STP.Printer

let rec repl () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | str -> let lex = new lexer str in
            let _ = lex#tokenize in
            let ast = parse_expr lex#get_tokens in
            print_expr ast;
      repl ();;

let () = repl ();;
