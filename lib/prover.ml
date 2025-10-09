open Lexer
open Printer
open Parser
open Printf

exception Solved_lemma of lemma

let apply_reflexivity (nm, comp) = 
  match comp with
  | Eq (expr1, expr2) -> if expr1 = expr2 then Solved_lemma (nm, comp) |> raise else (nm, comp);;

let rec simplify_expr expr = 

  let match_op op expr1 expr2 = 
    match op with
    | Add -> (simplify_expr expr2 + simplify_expr expr1)
    | Sub -> (simplify_expr expr2 - simplify_expr expr1)
    | Mult -> (simplify_expr expr2 * simplify_expr expr1) in
  match expr with
  | Binop (op, expr1, expr2) -> match_op op expr1 expr2
  | Num y -> y;;

let apply_simpl (nm, comp) = 
  match comp with
  | Eq (expr1, expr2) -> (nm, Eq (Num (simplify_expr expr1), Num (simplify_expr expr2)));;

let apply_tactic tac lemma : lemma = 
  match tac with
  | Reflexivity -> apply_reflexivity lemma
  | Simpl -> apply_simpl lemma

(*A repl inside the get_lemma repl*)
let rec prove_lemma (lemma : lemma) : unit = 
  print_string "*** ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Not solving lemma anymore!"
  | _ -> let lex = new lexer txt in
           let tokens = lex#tokenize [] in 
             let pars = new parse_tactic tokens in
               let tac = pars#parse_tactic in
                 let nlemma = apply_tactic tac lemma in
                 printf "%s\n" (flemma nlemma); prove_lemma nlemma;;

let interp str =
  try
    let lex = new lexer str in
      let tokens = lex#tokenize [] in 
        let pars = new parse_lemma tokens in
          let (nme, comp) = pars#parse_lemma in
            printf "\nSolving %s Lemma\n" nme;
            prove_lemma (nme, comp) 
  with 
  | Parsing_error err -> printf "%s\n" err
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks;;


let rec get_lemma () =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | _ -> try 
         interp txt; get_lemma ();
         with
         | Solved_lemma (nm, _) -> printf "\nSolved lemma %s! Do another!\n" nm; get_lemma ()
         | e -> Printexc.to_string e |> printf "ANOMALY: %s\n"; get_lemma ();;


