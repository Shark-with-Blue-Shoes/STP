val format_pos : Lexer.position -> string

val print_tokens : Lexer.token list -> unit

val format_op : Parser.op -> string

val format_peano : Parser.peano -> string

val format_expr : Parser.expr -> string

val print_expr : Parser.expr -> unit

val format_comp : Parser.comp -> string

val print_comp : Parser.comp -> unit

val format_bound_var : Parser.bound_var -> string

val format_bound_vars : Parser.bound_var list -> string

val format_vars : Parser.bound_var list -> string

val format_quantifier : Parser.quantifier -> string

val print_quantifier : Parser.quantifier -> unit

val format_lemma : Parser.lemma -> string

val print_lemma : Parser.lemma -> unit
