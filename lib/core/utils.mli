val free_vars : Syntax.term -> Syntax.var list

val subst :
  Syntax.term ->
  Syntax.var ->
  Syntax.term ->
  Syntax.term

val string_of_term : Syntax.term -> string
val term_of_string : string -> Syntax.term

val alpha_equiv : Syntax.term -> Syntax.term -> bool
