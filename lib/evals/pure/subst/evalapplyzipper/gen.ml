open Core.Syntax
open Core.Utils
open Printer

let rec gen la op1 ar1 op2 ar2 (t, z_ctxt) =
  let gen_aux = gen la op1 ar1 op2 ar2 in
  match t with
    | App (m, n) ->
      let (m',_) = op1 (m, AppL(z_ctxt, n)) in (
        match m' with
            | Abs (x, b) ->
              let (n',_) = ar1 (n, AppR(m', z_ctxt)) in
              let redex_str = string_of_redex (App(m', n')) in
              let full_str = plug_str redex_str z_ctxt in
              print_endline full_str;
              gen_aux (subst n' x b, z_ctxt)
            | _ ->
              let (m'',_) = op2 (m', AppL(z_ctxt, n)) in
              let (n',_) = ar2 (n, AppR(m'', z_ctxt)) in
                (App (m'', n'), z_ctxt)
      )
    | Abs (x, b) ->
      let (b', _) = la (b, AbsC(x,z_ctxt)) in
      (Abs(x, b'), z_ctxt)
    | Var _ as v -> (v, z_ctxt)
