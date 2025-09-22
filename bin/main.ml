open STP.Prover

open Printf

let rec get_lemma () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | _ -> try 
         interp_lemma txt; get_lemma ();
         with
         | Solved_lemma (nm, _) -> printf "Solved lemma %s! Do another!\n" nm; get_lemma ()
         | e -> Printexc.to_string e |> printf "ANOMALY: %s\n"; get_lemma ();;

let () = print_string "Welcome to the Simple Theorem Prover, or STP for short!\n\n";
         print_string "Insert a lemma to get started or insert exit to stop!\n"; 
         get_lemma ();;
