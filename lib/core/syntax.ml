type var = string

type term =
  | Var of var
  | Abs of var * term
  | App of term * term

type context =
  | Nihil
  | Bind of var * cterm * context
and cterm =
  | CVar of var
  | CAbs of var * cterm
  | CApp of cterm * cterm
  | Clou of cterm * context
