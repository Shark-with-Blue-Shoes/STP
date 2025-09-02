open STP.Lexer
open Printf

let rec repl () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | str -> let lex = new lexer str in
            let _ = lex#tokenize in
                lex#pretty_print;
      repl ();;

let read_file file : string = In_channel.with_open_bin file In_channel.input_all;;

let interp file = 
  print_string "interpreting file...\n";
  let str = read_file file in
    let lex = new lexer str in
      let _ = lex#tokenize in
      lex#pretty_print;;

let () = 
  try
    match Sys.argv.(1) with 
    | "repl" -> repl ()
    | str -> interp str
  with 
  | Invalid_argument _ -> printf "put a damn argument!";;
