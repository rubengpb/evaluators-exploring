open Core.Syntax
open Evals.Eval

type config = {
  eval : eval;
  env  : (string * pterm * pterm) list;
  church_num : bool;
  church_list : bool;
  display : bool;
  simulator : simulator option;
}

let string_of_config cfg =
  "{\n" ^ "  eval: " ^ string_of_eval cfg.eval ^ ";\n  env: [...]" ^
  ";\n  church_num: " ^ string_of_bool cfg.church_num ^
  ";\n  church_list: " ^ string_of_bool cfg.church_list ^
  ";\n  display: " ^ string_of_bool cfg.display ^
  ";\n  simulator: " ^ string_of_simulator_option cfg.simulator ^
  ";\n}"

let string_of_onoff = function
  | true -> "on"
  | false -> "off"

let string_of_church num list =
  "church_num = " ^ string_of_onoff num ^
  "\nchurch_list = " ^ string_of_onoff list
