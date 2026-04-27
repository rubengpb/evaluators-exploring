open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Value_utils
open Ho
open Printer

let rec so (t, zipp) =
  match t with
    | CVar x as v -> (v, zipp)
    | Clou(CVar x, []) -> (CVar x, zipp)
    | Clou(CVar x, (y, v)::env) ->
      if x = y then (v, zipp) else so @@ (Clou(CVar x, env), zipp)
    | CAbs(x, b) ->
      let (b', _) = so (b, CAbsC(x, zipp)) in (CAbs(x, b'), zipp)
    | Clou(CAbs(x, b), env) ->
      let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
      let (b', _) = so @@ (Clou(b, (x, CVar var)::env), CClouC(zipp, env)) in
        (CAbs(var, b'), zipp)
    | CApp(m, n) ->
      let (m', _) = ho (m, CAppL(zipp, n)) in (
        match m' with
          | CAbs(x, b) ->
            let (n', _) = so (n, CAppR(m', zipp)) in
            if is_cvalue n' then
            let redex_str = string_of_c_redex (CApp(m', n')) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            so @@ (Clou(b, [(x, n')]), zipp)
            else (CApp(m', n'), zipp)
          | Clou(CAbs(x, b), env) ->
            let (n', _) = so (n, CAppR(m', zipp)) in
            if is_cvalue n' then
            let redex_str = string_of_c_redex (CApp(m', n')) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            so @@ (Clou(b, (x, n')::env), zipp)
            else (CApp(m', n'), zipp)
          | _ ->
            let (m'', _) = so (m', CAppL(zipp, n)) in
            let (n', _) = so (n, CAppR(m'', zipp)) in
            (CApp(m', n'), zipp)
      )
    | Clou(CApp(m, n), env) ->
      let (m', _) = ho @@ (Clou(m, env), CAppL(zipp, n)) in (
        match m' with
          | CAbs(x, b) ->
            let (n', _) = so @@ (Clou(n, env), CAppR(m', zipp)) in
            if is_cvalue n' then
            let redex_str = string_of_c_redex (CApp(m', n')) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            so @@ (Clou(b, [(x, n')]), zipp)
            else (CApp(m', n'), zipp)
          | Clou(CAbs(x, b), env') ->
            let (n', _) = so @@ (Clou(n, env), CAppR(m', zipp)) in
            if is_cvalue n' then
            let redex_str = string_of_c_redex (CApp(m', n')) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            so @@ (Clou(b, (x, n')::env'), zipp)
            else (CApp(m', n'), zipp)
          | _ ->
            let (m'', _) = so @@ (m', CAppL(zipp, n)) in
            let (n', _) = so @@ (Clou(n, env), CAppR(m'', zipp)) in
            (CApp(m'', n'), zipp)
      )
    | Clou(Clou(t, env1), env2) -> so @@ (Clou(t, env1 @ env2), zipp)
