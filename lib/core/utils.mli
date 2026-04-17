val free_vars : Syntax.pterm -> Syntax.var list

val subst :
  Syntax.pterm ->
  Syntax.var ->
  Syntax.pterm ->
  Syntax.pterm
val subst_db :
  Syntax.dbterm ->
  int ->
  Syntax.dbterm ->
  Syntax.dbterm

val string_of_term : Syntax.term -> string
val string_of_pterm : Syntax.pterm -> string
val string_of_cterm : Syntax.cterm -> string
val string_of_ctxt : Syntax.context -> string
val string_of_dbterm : Syntax.dbterm -> string
val string_of_dbterm_pure : Syntax.dbterm -> string
val term_of_string : string -> Syntax.pterm
val clousure_of_pure : Syntax.pterm -> Syntax.cterm
val pterm_of_dbterm : Syntax.dbterm -> Syntax.pterm
val dbterm_of_pterm : Syntax.pterm -> Syntax.dbterm

val is_there_redex : Syntax.pterm -> bool

val alpha_equiv : Syntax.pterm -> Syntax.pterm -> bool
val plug : Syntax.pterm -> Syntax.zipper_ctxt -> Syntax.pterm
