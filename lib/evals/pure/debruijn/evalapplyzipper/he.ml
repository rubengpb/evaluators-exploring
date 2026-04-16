open Core.Syntax
open Core.Utils
open Printer

let rec he (t, z_ctxt) =
  match t with
    | FDBVar _ as v -> (v, z_ctxt)
    | DBVar _ as v -> (v, z_ctxt)
    | DBAbs b ->
      let (b', _) = he (b, DBAbsC(z_ctxt)) in
        (DBAbs(b'), z_ctxt)
    | DBApp (m, n) ->
      let (m', _) = he (m , DBAppL(z_ctxt, n)) in
      match m' with
        | DBAbs b ->
          let redex_str = string_of_db_redex (DBApp(m', n)) in
          let full_str = plug_db_str redex_str z_ctxt in
          print_endline full_str;
          he (subst_db n 0 b, z_ctxt)
        | _ -> (DBApp(m', n), z_ctxt)
