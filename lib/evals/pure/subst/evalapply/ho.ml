open Core.Syntax
open Core.Utils
open Printer

let rec ho = function
  | App (t1, t2) ->
    let t1' = ho t1 in (
      match t1' with
          | Abs (x, body) -> ho @@ subst (ho t2) x body
          | _ -> App (t1', t2)
    )
  | Abs(x, t) -> Abs (x, ho t)
  | Var x -> Var x

let rec ho_zipp (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = ho_zipp (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = ho_zipp (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let (n', _) = ho_zipp (n, AppR(m', z_ctxt)) in
          let redex_str = string_of_redex (App(m', n')) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          ho_zipp (subst n' x b, z_ctxt)
        | _ -> (App(m', n), z_ctxt)
