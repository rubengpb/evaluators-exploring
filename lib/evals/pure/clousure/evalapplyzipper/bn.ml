open Core.Syntax
open Printer

let rec bn (t, zipp) =
  match t with
    | CVar x as v -> (v, zipp)
    | Clou(CVar x, []) -> (CVar x, zipp)
    | Clou(CVar x, (y, n)::env) ->
        if x = y then bn (n, zipp) else bn @@ (Clou(CVar x, env), zipp)
    | CAbs(_,_) as abs -> (abs, zipp)
    | Clou(CAbs(_,_),_) as abs -> (abs, zipp)
    | CApp(m, n) ->
      let (m', _) = bn (m, CAppL(zipp, n)) in (
        match m' with
          | CAbs(x, b) ->
            let redex_str = string_of_c_redex (CApp(m', n)) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            bn @@ (Clou(b, [(x, n)]), zipp)
          | Clou(CAbs(x, b), env) ->
            let redex_str = string_of_c_redex (CApp(m', n)) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            bn @@ (Clou(b, (x, n)::env), zipp)
          | _ -> (CApp(m', n), zipp)
      )
    | Clou(CApp(m, n), env) ->
      let (m', _) = bn @@ (Clou(m, env), CAppL(zipp, n)) in (
        match m' with
          | CAbs(x, b) ->
            let redex_str = string_of_c_redex (Clou(CApp(m', n), env)) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            bn @@ (Clou(b, [(x, Clou(n, env))]), zipp)
          | Clou(CAbs(x, b), env') ->
            let redex_str = string_of_c_redex (Clou(CApp(m', n), env)) in
            let full_str = plug_c_str redex_str zipp in
            print_endline full_str;
            bn @@ (Clou(b, (x, Clou(n, env))::env'), zipp)
          | _ -> (CApp(m', Clou(n, env)), zipp)
      )
    (* | Clou(Clou(t, env1), env2) -> bn @@ Clou(t, env1 @ env2) *)
    | _ -> failwith "[Error] Evaluation of CallByName wiht Clousure"
