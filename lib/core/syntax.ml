type var = string

type pterm =
  | Var of var
  | Abs of var * pterm
  | App of pterm * pterm

type context = (var * cterm) list
and cterm =
  | CVar of var
  | CAbs of var * cterm
  | CApp of cterm * cterm
  | Clou of cterm * context

type dbterm =
  | FDBVar of string
  | DBVar of int
  | DBAbs of dbterm
  | DBApp of dbterm * dbterm

type term =
  | TPure of pterm
  | TClousure of cterm
  | TDeBruijn of dbterm

type zipper_ctxt =
  | Top
  | AppL of zipper_ctxt * pterm (* [] n *)
  | AppR of pterm * zipper_ctxt (* m [] *)
  | AbsC of string * zipper_ctxt

type zipper_ctxt_db =
  | DBTop
  | DBAppL of zipper_ctxt_db * dbterm
  | DBAppR of dbterm * zipper_ctxt_db
  | DBAbsC of zipper_ctxt_db

type zipper = term * context
