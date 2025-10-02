open STP.Lexer
open STP.Printer

let interp str =
  let lex = new lexer str in
    let (toks, _) = lex#tokenize [] |> List.split in 
      print_tokens toks;;

let rec repl () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | _ -> interp txt; repl ();;

let () = repl ();;
