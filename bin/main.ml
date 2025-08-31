
let rec repl () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | _ -> repl ();;

let () = repl ();;
