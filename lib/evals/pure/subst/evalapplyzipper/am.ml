open Core.Syntax
open Core.Utils
open Bv
open Printer

let rec am (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = am (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = bv (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let (n', _) = bv (n, AppR(m', z_ctxt)) in
          let redex_str = string_of_redex (App(m', n')) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          am (subst n' x b, z_ctxt)
        | _ -> let (n', _) = bv (n, AppR(m', z_ctxt)) in
            let (m'', _) = am (m', AppL(z_ctxt, n)) in
            (App(m'', n'), z_ctxt)
