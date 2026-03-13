open Evals.Eval
open Config

let handle_info st =
   print_endline @@ string_of_eval st.eval;
   st
