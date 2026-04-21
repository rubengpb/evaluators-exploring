open Core.Syntax
open Core.Utils
open Cl_utils
open Bv
open Printer

let rec am (t, zipp) =
  match t with
    | CVar x as v -> (v, zipp)
    | Clou(CVar x, []) -> (CVar x, zipp)
    | Clou(CVar x, (y, v)::env) ->
      if x = y then (v, zipp) else am @@ (Clou(CVar x, env), zipp)
    | CAbs(x, b) ->
      let (b', _) = am (b, CAbsC(x, zipp)) in (CAbs(x, b'), zipp)
    | Clou(CAbs(x, b), env) ->
      let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
      let (b', _) = am @@ (Clou(b, (x, CVar var)::env), CClouC(zipp, env)) in
        (CAbs(var, b'), zipp)
    | CApp(m, n) ->
      let (m', _) = bv (m, CAppL(zipp, n)) in (
        match m' with
          | CAbs(x, b) ->
            let (n', _) = bv (n, CAppR(m', zipp)) in
            let redex_str = string_of_c_redex (CApp(m', n')) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            am @@ (Clou(b, [(x, n')]), zipp)
          | Clou(CAbs(x, b), env) ->
            let (n', _) = bv (n, CAppR(m', zipp)) in
            let redex_str = string_of_c_redex (CApp(m', n')) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            am @@ (Clou(b, (x, n')::env), zipp)
          | _ ->
            let (m'', _) = am (m', CAppL(zipp, n)) in
            let (n', _) = bv (n, CAppR(m'', zipp)) in
            (CApp(m'', n'), zipp)
      )
    | Clou(CApp(m, n), env) ->
      let (m', _) = bv @@ (Clou(m, env), CAppL(zipp, n)) in (
        match m' with
          | CAbs(x, b) ->
            let (n', _) = bv @@ (Clou(n, env), CAppR(m', zipp)) in
            let redex_str = string_of_c_redex (CApp(m', n')) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            am @@ (Clou(b, [(x, n')]), zipp)
          | Clou(CAbs(x, b), env') ->
            let (n', _) = bv @@ (Clou(n, env), CAppR(m', zipp)) in
            let redex_str = string_of_c_redex (CApp(m', n')) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            am @@ (Clou(b, (x, n')::env'), zipp)
          | _ ->
            let (m'', _) = am @@ (m, CAppL(zipp, n)) in
            let (n', _) = bv @@ (Clou(n, env), CAppR(m'', zipp)) in
            (CApp(m', n'), zipp)
      )
    | Clou(Clou(t, env1), env2) -> am @@ (Clou(t, env1 @ env2), zipp)
