open Lexer

exception Parsing_error of string * position;;

type peano = 
  | O
  | S of peano;;

let rec num_to_peano (num : int) : peano = 
  match num with
  | 0 -> O
  | _ -> S (num_to_peano (num-1));;

type expr = 
  | Peano of peano
  | Binop of op * expr * expr

and op = 
  | Add
  | Sub
  | Mult
  | Div;;

let parse_op (token : Lexer.token) : op =
  let open Tokens in
  let tok = token.t in
    match tok with
    | MULT -> Mult
    | DIV -> Div
    | PLUS -> Add
    | SUB -> Sub
    | _ -> Parsing_error ("Wrong op!", token.pos) |> raise;;

let def_pos : Lexer.position = { line_num = 0; bol_off = 0; offset = 0};;

let parse_expr (tokens : Lexer.token list) : expr = 
  
  let get_num (token : Lexer.token) : int =
    match token.t with
    | Tokens.Num x -> x
    | _ -> Parsing_error ("Not a number", token.pos) |> raise in

  let get_eof (token: Lexer.token) : unit = 
    match token.t with
    | Tokens.EOF -> ()
    | _ -> Parsing_error ("How the hell is EOF not the last in the list?", token.pos) |> raise in

  let rec parse_binop (curr_expr : expr) (tokens : Lexer.token list) : expr =
    match tokens with
    | op :: num2 :: ls -> 
        let y = get_num num2 in
        ls |> parse_binop (Binop ((parse_op op), curr_expr, (Peano (num_to_peano y))))
    | [x] -> let _ = get_eof x in curr_expr
    | _ -> Parsing_error ("Where the helly is EOF?", def_pos) |> raise
    in

  match tokens with
  | [eof] ->  let _ = get_eof eof in Parsing_error ("End of file, but how?", def_pos) |> raise
  | hd :: ls -> let num = get_num hd in let n1 = Peano (num_to_peano num) in parse_binop n1 ls
  | [] -> Parsing_error ("Where the helly are the tokens", def_pos) |> raise;;
