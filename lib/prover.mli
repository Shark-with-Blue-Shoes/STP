exception Solved_lemma of Parser.lemma

val apply_reflexivity : string * Parser.comp -> string * Parser.comp

val apply_tactic :
  Parser.tactic -> string * Parser.comp -> Parser.lemma

val prove_lemma : Parser.lemma -> unit

val interp : string -> unit

val get_lemma : unit -> unit
