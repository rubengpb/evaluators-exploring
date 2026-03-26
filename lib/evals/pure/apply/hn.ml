open Core.Syntax
open Core.Utils
open He
open Printer

let rec eval_hn = function
  | App (t1, t2) ->
    let t1' = eval_he t1 in (
      match t1' with
          | Abs (x, body) -> eval_hn @@ subst t2 x body
          | _ -> App (eval_hn t1', eval_hn t2)
    )
  | Abs(x, t) -> Abs (x, eval_hn t)
  | Var x -> Var x

let rec eval_hn_zipp (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = eval_hn_zipp (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = eval_he_zipp (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let redex_str = string_of_redex (App(m', n)) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          eval_hn_zipp (subst n x b, z_ctxt)
        | _ -> let (m'', _) = eval_hn_zipp (m', AppL(z_ctxt, n)) in
          let (n', _) = eval_hn_zipp (n, AppR(m', z_ctxt)) in
            (App(m'', n'), z_ctxt)
