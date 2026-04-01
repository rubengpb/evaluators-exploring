open Evals.Eval
open Config

let handle_set st param opt =
  match param with
  | "env" -> { st with env = [] }
  | "language" -> (
    match language_of_string opt with
      | Some lang -> let ev = { st.eval with language = lang } in
        { st with eval = ev}
      | None ->
         print_endline @@ "[ERROR in set language]\nNon exists this language: " ^ opt;
         st
    )
  | "subst" -> (
    match subst_of_string opt with
      | Some sub -> let ev = { st.eval with subst = sub } in
        { st with eval = ev}
      | None ->
         print_endline @@ "[ERROR in set subst]\nNon exists this subst: " ^ opt;
         st
    )
  | "style" -> (
    match style_of_string opt with
      | Some sty -> let ev = { st.eval with style = sty } in
        { st with eval = ev}
      | None ->
         print_endline @@ "[ERROR in set style]\nNon exists this style: " ^ opt;
         st
    )
  | "strategy" -> (
    match strategy_of_string opt with
      | Some str -> let ev = { st.eval with strategy = str } in
        { st with eval = ev}
      | None ->
         print_endline @@ "[ERROR in set strategy]\nNon exists this strategy: " ^ opt;
         st
    )
  | "eval" ->
    (match eval_of_string opt with
    | Some ev -> { st with eval = ev }
    | None ->
        print_endline @@ "[ERROR in set eval]\nNon exists this eval: " ^ opt;
        st)
  | "church" ->
    if opt = "true" then { st with church = true }
    else { st with church = false }
  | "display" ->
    if opt = "true" then { st with display = true }
    else { st with display = false }
  | other ->
    print_endline @@ "[ERROR in set]\nNon exists this param: " ^ other;
    st
