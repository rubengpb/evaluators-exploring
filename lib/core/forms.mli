open Syntax

val spine : term -> term * term list

val is_neu : term -> bool
val is_nf : term -> bool
val is_wnf : term -> bool
val is_hnf : term -> bool
val is_whnf : term -> bool
val is_vhnf : term -> bool
