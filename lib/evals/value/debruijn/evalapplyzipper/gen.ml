open Core.Syntax
open Core.Utils
open Value_utils
open Printer

let rec gen la op1 ar1 op2 ar2 (t, z_ctxt) =
  let gen_aux = gen la op1 ar1 op2 ar2 in
  match t with
    | DBApp (m, n) ->
      let (m',_) = op1 (m, DBAppL(z_ctxt, n)) in (
        match m' with
            | DBAbs b ->
              let (n',_) = ar1 (n, DBAppR(m', z_ctxt)) in
              if is_dbvalue n' then
                let redex_str = string_of_db_redex (DBApp(m', n')) in
                let full_str = plug_db_str redex_str z_ctxt in
                print_endline full_str;
                gen_aux (subst_db n' 0 b, z_ctxt)
              else
                (DBApp(m', n'), z_ctxt)
            | _ ->
              let (m'',_) = op2 (m', DBAppL(z_ctxt, n)) in
              let (n',_) = ar2 (n, DBAppR(m'', z_ctxt)) in
                (DBApp (m'', n'), z_ctxt)
      )
    | DBAbs b ->
      let (b', _) = la (b, DBAbsC z_ctxt) in
      (DBAbs b', z_ctxt)
    | DBVar _ as v -> (v, z_ctxt)
    | FDBVar _ as v -> (v, z_ctxt)
