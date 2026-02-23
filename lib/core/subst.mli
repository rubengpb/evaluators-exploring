val free_vars : Syntax.term -> Syntax.var list

val subst :
  Syntax.term ->
  Syntax.var ->
  Syntax.term ->
  Syntax.term
