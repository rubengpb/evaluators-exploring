open Core.Syntax
open Core.Utils
open Bv
open Printer

let rec eval_sn = function
  | App (t1, t2) ->
    let t1' = eval_bv t1 in (
      match t1' with
          | Abs (x, body) -> eval_sn @@ subst (eval_bv t2) x body
          | _ -> App (eval_sn t1', eval_sn t2)
    )
  | Abs(x, t) -> Abs (x, eval_sn t)
  | Var x -> Var x

let rec eval_sn_zipp (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = eval_sn_zipp (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = eval_bv_zipp (m , AppL(z_ctxt, n)) in
      let (n', _) = eval_bv_zipp (n, AppR(m', z_ctxt)) in
      match m' with
        | Abs(x, b) ->
          let redex_str = string_of_redex (App(m', n')) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          eval_sn_zipp (subst n' x b, z_ctxt)
        | _ -> let (m'', _) = eval_sn_zipp (m', AppL(z_ctxt, n)) in
          let (n'', _) = eval_sn_zipp (n', AppR(m'', z_ctxt)) in
            (App(m'', n''), z_ctxt)
