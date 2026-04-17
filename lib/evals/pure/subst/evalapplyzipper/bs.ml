open Core.Syntax
open Core.Utils
open Printer
open Ho

let rec bs (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = bs (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = ho (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let (n', _) = ho (n, AppR(m', z_ctxt)) in
          let redex_str = string_of_redex (App(m', n')) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          bs (subst n' x b, z_ctxt)
        | _ -> let (m'', _) = bs (m' , AppL(z_ctxt, n)) in
          let (n', _) = bs (n, AppR(m', z_ctxt)) in
            (App(m'', n'), z_ctxt)
