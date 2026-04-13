open Syntax

val pterm_of_list : pterm list -> pterm

val list_of_pterm : pterm -> pterm list option
val list_of_term : term -> pterm list option
