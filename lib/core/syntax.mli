type var = string

type term =
  | Var of var
  | Abs of var * term
  | App of term * term
