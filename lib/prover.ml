open Lexer
open Parser
open Printer

open Printf

exception Solved_lemma of lemma

let apply_reflexivity (nm, comp) = 
  match comp with
  | Eq (expr1, expr2) -> if expr1 = expr2 then Solved_lemma (nm, comp) |> raise else (nm, comp);;

let apply_tactic ast lemma : lemma = 
  match ast with
  | Reflexivity -> apply_reflexivity lemma
  | Rewrite _ -> print_string "Can't rewrite yet\n"; lemma;;

(*Takes a lemma and a string, parses the string and applies the tactic to the lemma*)
let interp_tactic (str : string) (lemma : lemma) : lemma = 
  try
    let lex = new lexer str in
      let tokens = lex#tokenize [] in
        let ast = parse_input tokens parse_tactic in 
          let new_lemma = apply_tactic ast lemma in 
            let _ = print new_lemma flemma in new_lemma
  with 
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks; lemma
  | Parsing_error (err, tok) -> 
      let (tok, pos) = tok in
      let tokstr = format_tok tok in
      printf "PARSING ERROR: %s at token %s line %d, offset %d\n" err tokstr pos.line_num pos.bol_off;
      lemma

(*A repl inside the get_lemma repl*)
let rec prove_lemma (lemma : lemma) : unit = 
  print_string "*** ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | _ -> let _(*New lemma*) = interp_tactic txt lemma in prove_lemma lemma;;

let interp_lemma (str : string) : unit = 
  try
    let lex = new lexer str in
      let tokens = lex#tokenize [] in
        let ast = parse_input tokens parse_lemma in
          print_string "Time to prove it!\n";
          prove_lemma ast;
  with 
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks;
  | Parsing_error (err, tok) -> 
      let (tok, pos) = tok in
      let tokstr = format_tok tok in
      printf "PARSING ERROR: %s at token %s line %d, offset %d\n" err tokstr pos.line_num pos.bol_off;;

