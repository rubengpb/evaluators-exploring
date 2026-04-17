open Syntax

val spine : pterm -> pterm * pterm list
val dbspine : dbterm -> dbterm * dbterm list

val is_var : pterm -> bool
val is_abs : pterm -> bool
val is_value : pterm -> bool
val is_neu : pterm -> bool
val is_nf : pterm -> bool
val is_wnf : pterm -> bool
val is_hnf : pterm -> bool
val is_whnf : pterm -> bool
val is_vhnf : pterm -> bool

val is_dbvar : dbterm -> bool
val is_dbabs : dbterm -> bool
val is_dbvalue : dbterm -> bool
val is_dbneu : dbterm -> bool
val is_dbnf : dbterm -> bool
val is_dbwnf : dbterm -> bool
val is_dbhnf : dbterm -> bool
val is_dbwhnf : dbterm -> bool
val is_dbvhnf : dbterm -> bool
