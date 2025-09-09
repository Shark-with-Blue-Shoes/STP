open STP.Lexer
open STP.Parser
open STP.Printer

open Printf

let run_assistant str = 
  try
    let lex = new lexer str in
      let tokens = lex#tokenize [] in
        let ast = parse_expr tokens in
          print_expr ast;
  with 
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks;
  | Parsing_error (err, pos) -> 
      printf "PARSING ERROR: %s at line %d, offset %d\n" err pos.line_num pos.bol_off;
  | e -> Printexc.to_string e |> printf "ANOMALY: %s\n";;


let rec repl () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | _ -> run_assistant txt;
      repl ();;

let () = repl ();;
