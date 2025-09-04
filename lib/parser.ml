exception Parsing_error of string;;

type expr = 
  | Num of int
  | Binop of op * expr * expr

and op = 
  | Add
  | Sub
  | Mult
  | Div;;

let parse_op (token : Tokens.t) : op =
  let open Tokens in
    match token with
    | MULT -> Mult
    | DIV -> Div
    | PLUS -> Add
    | SUB -> Sub
    | _ -> Parsing_error "Wrong op!" |> raise;;


let parse_expr (tokens : Tokens.t list) : expr = 

  let rec parse_binop (start : expr) (tokens : Tokens.t list) : expr =
    match tokens with
    | op :: Tokens.Num y :: ls -> parse_binop (Binop ((parse_op op), start, (Num y))) ls
    | [EOF] -> start
    | _ -> Parsing_error "Anomalous binop" |> raise
    in

  match tokens with
  | [] -> Parsing_error "Where the helly are the tokens" |> raise
  | hd :: tl -> (match hd with
                | Tokens.Num x -> let n1 = Num x in parse_binop n1 tl
                | _ -> Parsing_error "Anomalous op" |> raise);;

