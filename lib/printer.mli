val fpos : Lexer.position -> string

val print_token : Lexer.token -> unit

val print_tokens : Lexer.token list -> unit

val fop : Parser.op -> string

val fpeano : Parser.peano -> string

val fexpr : Parser.expr -> string

val fcomp : Parser.comp -> string

val fbound_var : Parser.bound_var -> string

val fbound_vars : Parser.bound_var list -> string

val fvars : Parser.bound_var list -> string

val fquantifier : Parser.quantifier -> string

val flemma : string * Parser.comp -> string

val ftactic : Parser.tactic -> string

val print : 'a -> ('a -> string) -> unit
