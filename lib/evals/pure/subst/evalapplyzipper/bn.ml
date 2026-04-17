open Core.Syntax
open Core.Utils
open Printer

let rec bn (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs _ as abs -> (abs, z_ctxt)
    | App (m, n) ->
      let (m', _) = bn (m, AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let redex_str = string_of_redex (App(m', n)) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          bn (subst n x b, z_ctxt)
        | _ -> (App(m', n), z_ctxt)
