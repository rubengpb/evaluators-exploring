open Core.Syntax
open Evals.Eval

type config = {
  eval : eval;
  env  : (string * pterm * pterm) list;
  church : bool;
  display : bool;
}

let string_of_config cfg =
  "{\n" ^ "  eval: " ^ string_of_eval cfg.eval ^ ";\n  env: [...]" ^
  ";\n  church: " ^ string_of_bool cfg.church ^
  ";\n  display: " ^ string_of_bool cfg.display ^
  ";\n}"
