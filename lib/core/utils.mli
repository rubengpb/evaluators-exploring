val free_vars : Syntax.term -> Syntax.var list

val subst :
  Syntax.term ->
  Syntax.var ->
  Syntax.term ->
  Syntax.term

val string_of_term : Syntax.term -> string

val alpha_equiv : Syntax.term -> Syntax.term -> bool
