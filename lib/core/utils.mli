val free_vars : Syntax.pterm -> Syntax.var list

val subst :
  Syntax.pterm ->
  Syntax.var ->
  Syntax.pterm ->
  Syntax.pterm

val string_of_term : Syntax.term -> string
val string_of_pterm : Syntax.pterm -> string
val string_of_cterm : Syntax.cterm -> string
val term_of_string : string -> Syntax.pterm
val clousure_of_pure : Syntax.pterm -> Syntax.cterm

val is_there_redex : Syntax.pterm -> bool

val alpha_equiv : Syntax.pterm -> Syntax.pterm -> bool
