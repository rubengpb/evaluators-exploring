open Core.Syntax
open Core.Utils
open Printer

let rec eval_ao = function
  | App (t1, t2) ->
    let t1' = eval_ao t1 in (
      match t1' with
        | Abs (x, body) -> eval_ao @@ subst (eval_ao t2) x body
        | _ -> App(t1', eval_ao t2)
    )
  | Abs (x, t) -> Abs (x, eval_ao t)
  | Var x -> Var x

let rec eval_ao_zipp (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = eval_ao_zipp (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = eval_ao_zipp (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let (n', _) = eval_ao_zipp (n, AppR(m', z_ctxt)) in
          let redex_str = string_of_redex (App(m', n')) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          eval_ao_zipp (subst n' x b, z_ctxt)
        | _ -> let (n', _) = eval_ao_zipp (n, AppR(m', z_ctxt)) in
            (App(m', n'), z_ctxt)
