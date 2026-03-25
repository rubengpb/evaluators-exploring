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

type term =
  | TPure of pterm
  | TClousure of cterm

type zipper_ctxt =
  | Top
  | AppL of zipper_ctxt * pterm (* [] n *)
  | AppR of pterm * zipper_ctxt (* m [] *)
  | AbsC of string * zipper_ctxt

type zipper = term * context
