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
  | Pure of pterm
  | Clousure of cterm
