open Core.Syntax
open Core.Utils
open Printer
open Bv

let rec sn (t, z_ctxt) =
  match t with
    | FDBVar _ as v -> (v, z_ctxt)
    | DBVar _ as v -> (v, z_ctxt)
    | DBAbs b ->
      let (b', _) = sn (b, DBAbsC(z_ctxt)) in
        (DBAbs b', z_ctxt)
    | DBApp (m, n) ->
      let (m', _) = bv (m , DBAppL(z_ctxt, n)) in
      match m' with
        | DBAbs b ->
          let (n', _) = bv (n, DBAppR(m', z_ctxt)) in
          let redex_str = string_of_db_redex (DBApp(m', n')) in
          let full_str = plug_db_str redex_str z_ctxt in
          print_endline full_str;
          sn (subst_db n' 0 b, z_ctxt)
        | _ -> let (m'', _) = sn (m, DBAppL(z_ctxt, n)) in
            let (n', _) = sn (n, DBAppR(m', z_ctxt)) in
            (DBApp(m'', n'), z_ctxt)
