open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Value_utils
open Printer

let rec gen la op1 ar1 op2 ar2 (t, zipp) =
  let gen_aux = gen la op1 ar1 op2 ar2 in
  match t with
  | CVar x as v -> (v, zipp)
  | Clou(CVar x, []) -> (CVar x, zipp)
  | Clou(CVar x, (y, n)::env) ->
    if x = y then gen_aux (n, zipp)
    else gen_aux @@ (Clou(CVar x, env), zipp)
  | CAbs(x, b) -> let (b', _) = la (b, CAbsC(x, zipp)) in (CAbs(x, b'), zipp)
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let (b', _) = la @@ (Clou(b, (x, CVar var)::env), CClouC(zipp, env)) in
      (CAbs(var, b'), zipp)
  | CApp(m, n) ->
    let (m', _) = op1 (m, CAppL(zipp, n)) in (
      match m' with
        | CAbs(x, b) ->
          let (n', _) = ar1 (n, CAppR(m', zipp)) in
          if is_cvalue n' then
          let redex_str = string_of_c_redex (CApp(m', n')) in
          let full_str = plug_c_str redex_str zipp in
          print_endline full_str;
          gen_aux @@ (Clou(b, [(x, n)]), zipp)
          else (CApp(m',  n'), zipp)
        | Clou(CAbs(x, b), env) ->
          let (n', _) = ar1 (n, CAppR(m', zipp)) in
          if is_cvalue n' then
          let redex_str = string_of_c_redex (CApp(m', n')) in
          let full_str = plug_c_str redex_str zipp in
          print_endline full_str;
          gen_aux @@ (Clou(b, (x, n)::env), zipp)
          else (CApp(m',  n'), zipp)
        | _ ->
          let (m'', _) = op2 (m', CAppL(zipp, n)) in
          let (n', _) = ar2 (n, CAppR(m'', zipp)) in
          (CApp(m'', n'), zipp)
    )
  | Clou(CApp(m, n), env) ->
    let (m', _) = op1 @@ (Clou(m, env), CAppL(zipp, Clou(n, env))) in (
      match m' with
        | CAbs(x, b) ->
          let (n', _) = ar1 (Clou(n, env), CAppR(m', zipp)) in
          if is_cvalue n' then
          let redex_str = string_of_c_redex (CApp(m', n')) in
          let full_str = plug_c_str redex_str zipp in
          print_endline full_str;
          gen_aux @@ (Clou(b, [(x, n')]), zipp)
          else (CApp(m',  n'), zipp)
        | Clou(CAbs(x, b), env') ->
          let (n', _) = ar1 (Clou(n, env), CAppR(m', zipp)) in
          if is_cvalue n' then
          let redex_str = string_of_c_redex (CApp(m', n')) in
          let full_str = plug_c_str redex_str zipp in
          print_endline full_str;
          gen_aux @@ (Clou(b, (x, n')::env'), zipp)
          else (CApp(m',  n'), zipp)
        | _ ->
          let (m'', _) = op2 (m', CAppL(zipp, n)) in
          let (n', _) = ar2 (Clou(n, env), CAppR(m'', zipp)) in
          (CApp(m'', n'), zipp)
    )
  | Clou(Clou(t, env1), env2) -> gen_aux @@ (Clou(t, env1 @ env2), zipp)
