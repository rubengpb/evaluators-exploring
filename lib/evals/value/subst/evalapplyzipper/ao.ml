open Core.Syntax
open Core.Utils
open Value_utils
open Printer

let rec ao (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = ao (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = ao (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let (n', _) = ao (n, AppR(m', z_ctxt)) in
          if is_value n' then
            let redex_str = string_of_redex (App(m', n')) in
            let full_str = plug_str redex_str z_ctxt in
            print_endline full_str;
            ao (subst n' x b, z_ctxt)
          else
            (App(m', n'), z_ctxt)
        | _ -> let (n', _) = ao (n, AppR(m', z_ctxt)) in
            (App(m', n'), z_ctxt)
