open Syntax

val pterm_of_list : pterm list -> pterm

val list_of_pterm : pterm -> pterm list option
val list_of_dbterm : dbterm -> dbterm list option
val list_of_cterm : cterm -> cterm list option
val list_of_term : term -> term list option
