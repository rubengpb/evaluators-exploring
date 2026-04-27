open Core.Syntax
open Printer
open Value_utils

let rec bv (t, zipp) =
  match t with
    | CVar x as v -> (v, zipp)
    | Clou(CVar x, []) -> (CVar x, zipp)
    | Clou(CVar x, (y, v)::env) ->
      if x = y then (v, zipp) else bv @@ (Clou(CVar x, env), zipp)
    | CAbs(_,_) as abs -> (abs, zipp)
    | Clou(CAbs(_,_),_) as abs -> (abs, zipp)
    | CApp(m, n) ->
      let (m',_) = bv (m, CAppL(zipp, n)) in
      let (n',_) = bv (n, CAppR(m', zipp)) in (
        match m' with
          | CAbs(x, b) ->
            if is_cvalue n' then
            let redex_str = string_of_c_redex (CApp(m', n')) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            bv @@ (Clou(b, [(x, n')]), zipp)
            else (CApp(m', n'), zipp)
          | Clou(CAbs(x, b), env) ->
            if is_cvalue n' then
            let redex_str = string_of_c_redex (CApp(m', n')) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            bv @@ (Clou(b, (x, n')::env), zipp)
            else (CApp(m', n'), zipp)
          | _ -> (CApp(m', n'), zipp)
      )
    | Clou(CApp(m, n), env) ->
      let (m',_) = bv @@ (Clou(m, env), zipp) in
      let (n',_) = bv @@ (Clou(n, env), zipp) in (
        match m' with
          | CAbs(x, b) -> 
            if is_cvalue n' then
            let redex_str = string_of_c_redex (Clou(CApp(m', n'), env)) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            bv @@ (Clou(b, [(x, n')]), zipp)
            else (CApp(m', n'), zipp)
          | Clou(CAbs(x, b), env) -> 
            if is_cvalue n' then
            let redex_str = string_of_c_redex (Clou(CApp(m', n'), env)) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            bv @@ (Clou(b, (x, n')::env), zipp)
            else (CApp(m', n'), zipp)
          | _ -> (CApp(m', n'), zipp)
      )
    | Clou(Clou(t, env1), env2) -> bv @@ (Clou(t, env1 @ env2), zipp)
