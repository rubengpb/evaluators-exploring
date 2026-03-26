open Core.Syntax
open Core.Utils
open Ho
open Printer

let rec eval_so = function
  | App (t1, t2) ->
    let t1' = eval_ho t1 in (
      match t1' with
          | Abs (x, body) -> eval_so @@ subst (eval_so t2) x body
          | _ -> App (eval_so t1', eval_so t2)
    )
  | Abs(x, t) -> Abs (x, eval_so t)
  | Var x -> Var x

let rec eval_so_zipp (t, z_ctxt) =
  match t with
    | Var _ as v -> (v, z_ctxt)
    | Abs (x, b) ->
      let (b', _) = eval_so_zipp (b, AbsC(x, z_ctxt)) in
        (Abs(x, b'), z_ctxt)
    | App (m, n) ->
      let (m', _) = eval_ho_zipp (m , AppL(z_ctxt, n)) in
      let (n', _) = eval_so_zipp (n, AppR(m', z_ctxt)) in
      match m' with
        | Abs(x, b) ->
          let redex_str = string_of_redex (App(m', n')) in
          let full_str = plug_str redex_str z_ctxt in
          print_endline full_str;
          eval_so_zipp (subst n' x b, z_ctxt)
        | _ -> let (m'', _) = eval_so_zipp (m' , AppL(z_ctxt, n)) in
            (App(m'', n'), z_ctxt)
