open Core.Syntax
open Core.Utils
open Value_utils
open Printer

let rec ho (t, z_ctxt) =
  match t with
    | FDBVar _ as v -> (v, z_ctxt)
    | DBVar _ as v -> (v, z_ctxt)
    | DBAbs b ->
      let (b', _) = ho (b, DBAbsC(z_ctxt)) in
        (DBAbs(b'), z_ctxt)
    | DBApp (m, n) ->
      let (m', _) = ho (m , DBAppL(z_ctxt, n)) in
      match m' with
        | DBAbs b ->
          let (n', _) = ho (n, DBAppR(m', z_ctxt)) in
          if is_dbvalue n' then
            let redex_str = string_of_db_redex (DBApp(m', n')) in
            let full_str = plug_db_str redex_str z_ctxt in
            print_endline full_str;
            ho (subst_db n' 0 b, z_ctxt)
          else
            (DBApp(m', n'), z_ctxt)
        | _ -> (DBApp(m', n), z_ctxt)
