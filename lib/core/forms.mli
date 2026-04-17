open Syntax

val spine : pterm -> pterm * pterm list

val is_var : pterm -> bool
val is_abs : pterm -> bool
val is_value : pterm -> bool
val is_neu : pterm -> bool
val is_nf : pterm -> bool
val is_wnf : pterm -> bool
val is_hnf : pterm -> bool
val is_whnf : pterm -> bool
val is_vhnf : pterm -> bool
