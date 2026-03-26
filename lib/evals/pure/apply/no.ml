open Core.Syntax
open Core.Utils
open Bn
open Printer

let rec eval_no = function
  | App (t1, t2 ) ->
    let t1' = eval_bn t1 in (
      match t1' with
      | Abs(x, body) -> eval_no @@ subst t2 x body
      | _ -> App (eval_no t1', eval_no t2)
    )
  | Abs (x, t) -> Abs (x, eval_no t)
  | Var x -> Var x

let rec eval_no_zipp (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = eval_no_zipp (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = eval_bn_zipp (m , AppL(z_ctxt, n)) in
      match m' with
        | Abs(x, b) ->
          let redex_str = string_of_redex (App(m', n)) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          eval_no_zipp (subst n x b, z_ctxt)
        | _ -> let (m'',_) = eval_no_zipp (m', z_ctxt) in
            let (n', _) = eval_no_zipp (n, AppR(m'', z_ctxt)) in
            (App(m'', n'), z_ctxt)
