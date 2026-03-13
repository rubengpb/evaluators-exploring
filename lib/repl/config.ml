open Core.Syntax
open Evals.Eval

type config = {
  eval : eval;
  env  : (string * term) list;
  church : bool;
  display : bool;
}
