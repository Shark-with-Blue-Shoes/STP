open Printf
open Parser
open Lexer


let format_pos (p: position) = 
  sprintf "line num: %d, offset: %d" p.line_num p.bol_off;;

let rec print_tokens toks : unit =
  let print_token (tok : token) : unit = 
    let (t, _) = tok in
    format_tok t |> printf "%s\n";
in
match toks with
| [] -> ()
| hd :: tl ->
    print_token hd;
    print_tokens tl;;

let format_op op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "//";;

let rec format_peano (p : peano) : string =
  match p with
  | S p -> format_peano p |> sprintf "S (%s)" 
  | O -> "O";;

let rec format_expr expr = 
  match expr with 
  | Peano n -> format_peano n |> sprintf "Peano(%s)"
  | Binop (op, expr1, expr2) -> 
      sprintf "Binop(%s, %s, %s)" (format_op op) (format_expr expr1) (format_expr expr2);;

let print_expr (expr : expr) =
   format_expr expr |> printf "%s\n";;

let format_comp comp = 
  match comp with
  | Eq (expr1, expr2) -> sprintf "Eq(%s, %s)" (format_expr expr1) (format_expr expr2);;

let print_comp (comp : comp) =
  format_comp comp |> printf "%s\n";;

let format_bound_var var =
  match var with
  | Bound_Var str -> sprintf "Var(%s)" str;;

let rec format_bound_vars vars = 
  match vars with
  | var :: ls -> (format_bound_vars ls) |> sprintf ", %s%s" (format_bound_var var)
  | [] -> "";;

let format_vars vars =
  match vars with
  | var :: ls -> format_bound_vars ls |> sprintf "%s%s" (format_bound_var var)
  | _ -> ""

let format_quantifier quant =
  match quant with
  | Existential ls -> format_vars ls |> sprintf "EXISTS(%s)" 
  | Universal ls -> format_vars ls |> sprintf "FORALL(%s)";;

let print_quantifier (quant : quantifier) =
  format_quantifier quant |> printf "%s\n";;

let format_lemma (nm, comp) =  
  format_comp comp |> sprintf "LEMMA (%s, %s)" nm;;

let print_lemma lemma = 
  format_lemma lemma |> printf "%s\n";;

let format_tactic tac = 
  match tac with
  | Reflexivity -> "Reflexivity"

let print_tactic tac = 
  format_tactic tac |> printf "%s\n";;
