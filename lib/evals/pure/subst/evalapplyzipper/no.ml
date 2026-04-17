open Core.Syntax
open Core.Utils
open Printer
open Bn

let rec no (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = no (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = bn (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let redex_str = string_of_redex (App(m', n)) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          no (subst n x b, z_ctxt)
        | _ -> let (m'',_) = no (m', AppL(z_ctxt, n)) in
            let (n', _) = no (n, AppR(m'', z_ctxt)) in
            (App(m'', n'), z_ctxt)
