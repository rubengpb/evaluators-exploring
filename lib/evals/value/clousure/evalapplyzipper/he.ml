open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Value_utils
open Printer

let rec he (t, zipp) =
  match t with
    | CVar x as v -> (v, zipp)
    | Clou(CVar x, []) -> (CVar x, zipp)
    | Clou(CVar x, (y, v)::env) ->
      if x = y then (v, zipp) else he @@ (Clou(CVar x, env), zipp)
    | CAbs(x, b) ->
      let (b', _) = he (b, CAbsC(x, zipp)) in (CAbs(x, b'), zipp)
    | Clou(CAbs(x, b), env) ->
      let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
      let (b', _) = he @@ (Clou(b, (x, CVar var)::env), CClouC(zipp, env)) in
        (CAbs(var, b'), zipp)
    | CApp(m, n) ->
      let (m', _) = he (m, CAppL(zipp, n)) in (
        match m' with
          | CAbs(x, b) ->
            if is_cvalue n then
            let redex_str = string_of_c_redex (CApp(m', n)) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            he @@ (Clou(b, [(x, n)]), zipp)
            else (CApp(m', n), zipp)
          | Clou(CAbs(x, b), env) ->
            if is_cvalue n then
            let redex_str = string_of_c_redex (CApp(m', n)) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            he @@ (Clou(b, (x, n)::env), zipp)
            else (CApp(m', n), zipp)
          | _ -> (CApp(m', n), zipp)
      )
    | Clou(CApp(m, n), env) ->
      let (m', _) = he @@ (Clou(m, env), CAppL(zipp, n)) in (
        match m' with
          | CAbs(x, b) ->
            if is_cvalue n then
            let redex_str = string_of_c_redex (CApp(m', Clou(n, env))) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            he @@ (Clou(b, [(x, Clou(n, env))]), zipp)
            else (CApp(m', n), zipp)
          | Clou(CAbs(x, b), env') ->
            if is_cvalue n then
            let redex_str = string_of_c_redex (CApp(m', Clou(n, env))) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            he @@ (Clou(b, (x, Clou(n, env))::env'), zipp)
            else (CApp(m', n), zipp)
          | _ -> (CApp(m', Clou(n, env)), zipp)
      )
    | Clou(Clou(t, env1), env2) -> he @@ (Clou(t, env1 @ env2), zipp)
