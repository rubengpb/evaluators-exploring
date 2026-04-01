open Core.Syntax
open Core.Utils
open Bv
open Printer

let rec ha = function
  | App (t1, t2) ->
    let t1' = bv t1 in (
      match t1' with
          | Abs (x, body) -> ha @@ subst (ha t2) x body
          | _ -> App (ha t1', ha t2)
    )
  | Abs(x, t) -> Abs (x, ha t)
  | Var x -> Var x

let rec ha_zipp (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = ha_zipp (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = bv_zipp (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let (n', _) = ha_zipp (n, AppR(m', z_ctxt)) in
          let redex_str = string_of_redex (App(m', n')) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          ha_zipp (subst n' x b, z_ctxt)
        | _ -> let (m'', _) = ha_zipp (m', AppL(z_ctxt, n)) in
          let (n', _) = ha_zipp (n, AppR(m', z_ctxt)) in
            (App(m'', n'), z_ctxt)
