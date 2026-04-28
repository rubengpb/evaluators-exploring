open Repl.Main_repl
open Repl.Config
open Evals.Eval

let initial_config = {
  eval = { language = Pure; subst = Subst; style = EvalApply; strategy = One NormalOrder; };
  env = [];
  church_num = false;
  church_list = false;
  display = false;
  simulator = None;
}

let () =
  Sys.catch_break true;
  print_endline @@ "λ-REPL evaluators-exploring, version 0.4.5:  " ^
    ":h for help,  :q for exit";
  loop initial_config
