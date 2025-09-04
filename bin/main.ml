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
  | Lexing_error (err, toks, pos) -> printf "LEXING ERROR: %s\n\n" err;
                                printf "line: %d, offset: %d\n\n\n" pos.line_num pos.bol_off;
                                print_string "Printing retrieved tokens...\n\n";
                                print_tokens toks;
  | e -> Printexc.to_string e |> printf "ANOMALY: %s\n";;


let rec repl () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | _ -> run_assistant txt;
      repl ();;

let read_file file : string = In_channel.with_open_bin file In_channel.input_all;;

let interp file = 
  print_string "interpreting file...\n";
  let str = read_file file in
  run_assistant str;; 

let () =
  try
    match Sys.argv.(1) with 
    | "repl" -> repl ()
    | str -> interp str
  with 
  | Invalid_argument _ -> printf "put a damn argument!"
