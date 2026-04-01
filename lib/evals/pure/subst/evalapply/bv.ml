open Core.Syntax
open Core.Utils
open Printer

let rec bv = function
  | App (t1, t2) ->
    let t1' = bv t1 in (
      match t1' with
          | Abs (x, body) -> bv @@ subst (bv t2) x body
          | _ -> App (t1', bv t2)
    )
  | t -> t

let rec bv_zipp (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) as abs -> (abs, z_ctxt)
    | App (m, n) ->
      let (m', _) = bv_zipp (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let (n', _) = bv_zipp (n, AppR(m', z_ctxt)) in
          let redex_str = string_of_redex (App(m', n')) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          bv_zipp (subst n' x b, z_ctxt)
        | _ -> let (n', _) = bv_zipp (n, AppR(m', z_ctxt)) in
            (App(m', n'), z_ctxt)
