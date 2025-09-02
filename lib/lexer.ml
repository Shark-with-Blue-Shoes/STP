exception Lexing_error of string;;

open Printf
open Tokens

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf;;


let string_to_tok str : Tokens.t = 
  match str with
  | "match" -> MATCH
  | "with" -> WITH
  | "if" -> IF
  | "else" -> ELSE
  | "true" -> TRUE
  | "false" -> FALSE
  | "lemma" -> LEMMA
  | "forall" -> FORALL
  | "nat" -> NAT
  | "definition" -> DEFINITION
  | _ -> Var str;;

class position_tracker txt =
  object
    val mutable pos = 0

    method next_pos () =
      pos <- pos + 1

    method back_pos () =
      pos <- pos - 1

    method current_pos = pos

    method at_eof () : bool = pos >= String.length txt
  end

class lexer (str : string) = object (self)
  (*The string being lexed*)
  val txt : string = str
 
  val pos_tracker = new position_tracker str 
  val mutable tokens = []
  
  (*Tokenizes txt*)
  method tokenize : unit =

  let tokenize_next () = pos_tracker#next_pos (); self#tokenize in

  (*This tokenizes numbers, it is initiated when the lexer finds a num*)
  let rec tokenize_num (chars : char list) =
    pos_tracker#next_pos ();
    if pos_tracker#at_eof () |> not then begin
      let char = txt.[pos_tracker#current_pos] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' -> Lexing_error "Can't end number with letter, add a space or something" |> raise 
      | '0' .. '9' -> tokenize_num (char :: chars)
      | _ -> pos_tracker#back_pos ();
            let final_num = List.rev chars |> string_of_chars |> int_of_string in
            let token = Num final_num in token end
    else
      let final_num = List.rev chars |> string_of_chars |> int_of_string in
        let token = Num final_num in token
          in

  let rec tokenize_word (chars : char list) : Tokens.t = 
    pos_tracker#next_pos ();
    if pos_tracker#at_eof () |> not then begin
      let char = txt.[pos_tracker#current_pos] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> tokenize_word (char :: chars)
      | _ -> pos_tracker#back_pos (); let token = List.rev chars |> string_of_chars |> string_to_tok in token end
    else 
      let token = List.rev chars |> string_of_chars |> string_to_tok in token
          in

  let rec skip_comment () : unit = 
    pos_tracker#next_pos ();
    if pos_tracker#at_eof () |> not then begin
      let char = txt.[pos_tracker#current_pos] in
      match char with
      | '\\' -> ()
      | _ -> skip_comment ()
    end
    else 
    Lexing_error "Forgot to close comment" |> raise in

  if pos_tracker#at_eof () |> not then begin
    try
      let char = txt.[pos_tracker#current_pos] in
      match char with
      | ' ' | '\t' | '\n' | '\r' -> tokenize_next ()
      | '\\' -> skip_comment (); tokenize_next ()
      | _ -> let token = match char with
                         | 'a' .. 'z' | 'A' .. 'Z' ->  tokenize_word [char]
                         | '0' .. '9' -> tokenize_num [char]
                         | '+' -> PLUS
                         | '/' -> DIV
                         | '*' -> MULT
                         | '-' -> SUB 
                         | '=' -> EQ
                         | '(' -> LPAREN
                         | ')' -> RPAREN
                         | '{' -> LBRACE
                         | '}' -> RBRACE
                         | '[' -> LBRACK
                         | ']' -> RBRACK
                         | ';' -> SEMICOLON
                         | ':' -> COLON
                         | '&' -> AND
                         | '|' -> OR
                         | ',' -> COMMA
                         | '.' -> PERIOD
                         | _ -> Lexing_error "Does not match any known char" |> raise in
                              tokens <- token :: tokens;
                                  tokenize_next ()
    with 
    | Lexing_error err -> 
        printf "LEXING ERROR: %s\nat offset: %i\n\n\nPrinting retrieved tokens...\n" err pos_tracker#current_pos; 
        tokens <- List.rev tokens;
    | err -> 
        Printexc.to_string err |> printf "ANOMALY: %s\n\n\nPrinting retrieved tokens...\n\n";
        tokens <- List.rev tokens; end
  else 
    tokens <- (EOF :: tokens) |> List.rev 

  method pretty_print : unit =
    let rec print_tokens toks : unit =
      let print_token (tok : t) : unit = 
        let str = 
        match tok with
        | Num i -> sprintf "NUM(%i)\n" i
        | Var s -> sprintf "VAR(%s)\n" s
        | MULT -> "MULT\n"
        | DIV -> "DIV\n"
        | PLUS -> "PLUS\n"
        | SUB -> "SUB\n"
        | EQ -> "EQ\n"
        | LPAREN -> "LPAREN\n"
        | RPAREN -> "RPAREN\n"
        | LBRACE -> "LBRACE\n"
        | RBRACE -> "RBRACE\n"
        | LBRACK -> "LBRACK\n"
        | RBRACK -> "RBRACK\n"
        | SEMICOLON -> "SEMICOLON\n"
        | COLON -> "COLON\n"
        | AND -> "AND\n"
        | OR -> "OR\n"
        | MATCH -> "MATCH\n"
        | WITH -> "WITH\n"
        | IF -> "IF\n"
        | ELSE -> "ELSE\n"
        | TRUE -> "TRUE"
        | FALSE -> "FALSE\n"
        | LEMMA -> "LEMMA\n"
        | FORALL -> "FORALL\n"
        | COMMA -> "COMMA\n"
        | PERIOD -> "PERIOD\n"
        | DEFINITION -> "DEFINITION\n"
        | NAT -> "NAT\n"
        | EOF -> "EOF\n" in print_string str;
    in
    match toks with
    | [] -> ()
    | hd :: tl ->
        print_token hd;
        print_tokens tl in 
    print_tokens tokens
end

