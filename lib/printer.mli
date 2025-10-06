val format_tok : Tokens.t -> string

val print_tokens : Tokens.t list -> unit

val fop : Parser.op -> string

val fexpr : Parser.expr -> string

val fcomp : Parser.comp -> string

val flemma : string * Parser.comp -> string

val print : ('a -> string) -> 'a -> unit
