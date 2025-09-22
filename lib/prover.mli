exception Solved_lemma of Parser.lemma

val apply_reflexivity : string * Parser.comp -> string * Parser.comp

val apply_rewrite : 'a * 'b -> 'a * 'b

val apply_tactic : Parser.tactic -> string * Parser.comp -> Parser.lemma

val interp_tactic : string -> Parser.lemma -> Parser.lemma

val prove_lemma : Parser.lemma -> unit

val interp_lemma : string -> unit

val get_lemma : unit -> unit
