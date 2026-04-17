open Syntax

val pterm_of_int : int -> pterm
val term_of_int : int -> term

val int_of_pterm : pterm -> int option
val int_of_dbterm : dbterm -> int option
val int_of_cterm : cterm -> int option
val int_of_term : term -> int option
