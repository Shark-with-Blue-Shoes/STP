exception Parsing_error of string * Lexer.position;;

type peano = 
  | O
  | S of peano;;

let rec num_to_peano (num : int) : peano = 
  if num = 0 |> not then 
    S (num_to_peano (num-1))
  else
    O;;

type expr = 
  | Peano of peano
  | Binop of op * expr * expr

and op = 
  | Add
  | Sub
  | Mult
  | Div;;

let parse_op (l : Lexer.lexed) : op =
  let open Tokens in
  let token = l.token in
    match token with
    | MULT -> Mult
    | DIV -> Div
    | PLUS -> Add
    | SUB -> Sub
    | _ -> Parsing_error ("Wrong op!", l.pos) |> raise;;

let def_pos : Lexer.position = { line_num = 0; bol_off = 0; offset = 0};;

let parse_expr (tokens : Lexer.lexed list) : expr = 
  
  let get_num (l : Lexer.lexed) : int =
    match l.token with
    | Tokens.Num x -> x
    | _ -> Parsing_error ("Not a number", l.pos) |> raise in

  let get_eof (l: Lexer.lexed) : unit = 
    match l.token with
    | Tokens.EOF -> ()
    | _ -> Parsing_error ("How the hell is EOF not the last in the list?", l.pos) |> raise in

  let rec parse_binop (curr_expr : expr) (tokens : Lexer.lexed list) : expr =
    match tokens with
    | op :: num2 :: ls -> 
        let y = get_num num2 in
        parse_binop (Binop ((parse_op op), curr_expr, (Peano (num_to_peano y)))) ls
    | [x] -> let _ = get_eof x in curr_expr
    | _ -> Parsing_error ("Where the helly is EOF?", def_pos) |> raise
    in

  match tokens with
  | [x] ->  let _ = get_eof x in Parsing_error ("End of file, but how?", def_pos) |> raise
  | hd :: tl -> let x = get_num hd in let n1 = Peano (num_to_peano x) in parse_binop n1 tl
  | [] -> Parsing_error ("Where the helly are the tokens", def_pos) |> raise;;
