open STP.Lexer
open STP.Printer
open STP.Parser
open Printf

let interp str =
  try
    let lex = new lexer str in
      let tokens = lex#tokenize [] in 
        let pars = new parsing tokens in
          let ast = pars#parse_comp in
          print_comp ast
  with 
  | Parsing_error err -> printf "%s\n" err
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks;;


let rec repl () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | _ -> interp txt; repl ();;

let () = repl ();;
