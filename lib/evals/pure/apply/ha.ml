open Core.Syntax
open Core.Utils
open Bv
open Printer

let rec eval_ha = function
  | App (t1, t2) ->
    let t1' = eval_bv t1 in (
      match t1' with
          | Abs (x, body) -> eval_ha @@ subst (eval_ha t2) x body
          | _ -> App (eval_ha t1', eval_ha t2)
    )
  | Abs(x, t) -> Abs (x, eval_ha t)
  | Var x -> Var x

let rec eval_ha_zipp (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = eval_ha_zipp (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = eval_bv_zipp (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let (n', _) = eval_ha_zipp (n, AppR(m', z_ctxt)) in
          let redex_str = string_of_redex (App(m', n')) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          eval_ha_zipp (subst n' x b, z_ctxt)
        | _ -> let (m'', _) = eval_ha_zipp (m', AppL(z_ctxt, n)) in
          let (n', _) = eval_ha_zipp (n, AppR(m', z_ctxt)) in
            (App(m'', n'), z_ctxt)
