open Printf
open Parser
open Lexer


let fpos (p: position) = 
  sprintf "line num: %d, offset: %d" p.line_num p.bol_off;;

let print_token (t,_) : unit = 
  format_tok t |> printf "%s\n";;

let rec print_tokens toks : unit =
  match toks with
  | [] -> ()
  | hd :: tl ->
    print_token hd;
    print_tokens tl;;

let fop op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "//";;

let rec fpeano (p : peano) : string =
  match p with
  | S p -> fpeano p |> sprintf "S (%s)" 
  | O -> "O";;

let rec fexpr expr = 
  match expr with 
  | Peano n -> fpeano n |> sprintf "Peano(%s)"
  | Binop (op, expr1, expr2) -> 
      sprintf "Binop(%s, %s, %s)" (fop op) (fexpr expr1) (fexpr expr2);;

let fcomp comp = 
  match comp with
  | Eq (expr1, expr2) -> sprintf "Eq(%s, %s)" (fexpr expr1) (fexpr expr2);;

let fbound_var var =
  match var with
  | Bound_Var str -> sprintf "Var(%s)" str;;

let rec fbound_vars vars = 
  match vars with
  | var :: ls -> (fbound_vars ls) |> sprintf ", %s%s" (fbound_var var)
  | [] -> "";;

let fvars vars =
  match vars with
  | var :: ls -> fbound_vars ls |> sprintf "%s%s" (fbound_var var)
  | _ -> ""

let fquantifier quant =
  match quant with
  | Existential ls -> fvars ls |> sprintf "EXISTS(%s)" 
  | Universal ls -> fvars ls |> sprintf "FORALL(%s)";;

let flemma (nm, comp) =  
  fcomp comp |> sprintf "LEMMA (%s, %s)" nm;;

let ftactic tac = 
  match tac with
  | Reflexivity -> "Reflexivity"
  | Rewrite str -> sprintf "Rewrite(%s)" str;;

let print (node : 'a) (format : 'a -> string) = 
  format node |> printf "%s\n";;
