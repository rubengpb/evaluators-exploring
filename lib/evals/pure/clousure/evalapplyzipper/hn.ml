open Core.Syntax
open Core.Utils
open Cl_utils
open He
open Printer

let rec hn (t, zipp) =
  match t with
    | CVar x as v -> (v, zipp)
    | Clou(CVar x, []) -> (CVar x, zipp)
    | Clou(CVar x, (y, v)::env) ->
      if x = y then (v, zipp) else hn @@ (Clou(CVar x, env), zipp)
    | CAbs(x, b) ->
      let (b', _) = hn (b, CAbsC(x, zipp)) in (CAbs(x, b'), zipp)
    | Clou(CAbs(x, b), env) ->
      let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
      let (b', _) = hn @@ (Clou(b, (x, CVar var)::env), CClouC(zipp, env)) in
        (CAbs(var, b'), zipp)
    | CApp(m, n) ->
      let (m', _) = he (m, CAppL(zipp, n)) in (
        match m' with
          | CAbs(x, b) ->
            let redex_str = string_of_c_redex (CApp(m', n)) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            hn @@ (Clou(b, [(x, n)]), zipp)
          | Clou(CAbs(x, b), env) ->
            let redex_str = string_of_c_redex (CApp(m', n)) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            hn @@ (Clou(b, (x, n)::env), zipp)
          | _ ->
            let (m'', _) = hn (m', CAppL(zipp, n)) in
            (CApp(m'', n), zipp)
      )
    | Clou(CApp(m, n), env) ->
      let (m', _) = he @@ (Clou(m, env), CAppL(zipp, n)) in (
        match m' with
          | CAbs(x, b) ->
            let redex_str = string_of_c_redex (CApp(m', Clou(n, env))) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            hn @@ (Clou(b, [(x, Clou(n, env))]), zipp)
          | Clou(CAbs(x, b), env') ->
            let redex_str = string_of_c_redex (CApp(m', Clou(n, env))) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            hn @@ (Clou(b, (x, Clou(n, env))::env'), zipp)
          | _ ->
            let (m'', _) = hn @@ (m', CAppL(zipp, n)) in
            (CApp(m'', n), zipp)
      )
    | Clou(Clou(t, env1), env2) -> hn @@ (Clou(t, env1 @ env2), zipp)
