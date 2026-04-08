open Core.Syntax
open Core.Utils
open Printer
open He

let rec hn (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = hn (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = he (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let redex_str = string_of_redex (App(m', n)) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          hn (subst n x b, z_ctxt)
        | _ -> let (m'', _) = hn (m', AppL(z_ctxt, n)) in
          let (n', _) = hn (n, AppR(m', z_ctxt)) in
            (App(m'', n'), z_ctxt)
