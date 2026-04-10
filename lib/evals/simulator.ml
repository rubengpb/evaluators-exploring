open Core.Syntax
open Core.Utils
open Eval

let rec sim_bn_bv t =
  let t' =
  match t with
    | Var x as v -> v
    | Abs(x, b) -> Abs("+", App(Var "+", b))
    | App(m, n) -> 
      let m', n' = sim_bn_bv m, sim_bn_bv n in
      Abs("+", App(m', (Abs("*", App(App(Var "*", n'), Var"+")))))
  in
  App(t', Abs("x", Var "x"))

let rec sim_bv_bn t =
  let t' =
  match t with
    | Var x as v -> Abs("*", App(Var "*", v))
    | Abs(x, b) ->
      let b' = sim_bv_bn b in
      Abs("*", App(Var "*", Abs(x, b')))
    | App(m, n) ->
      let m', n' = sim_bv_bn m, sim_bv_bn n in
      Abs("*", App(m', Abs("+", App(n', Abs("-", App(App(Var "+", Var "-"), Var "*"))))))
  in
  App(t', Abs("x", Var "x"))

let simulation_transform sim t =
  match sim.guest, sim.host with
    | CallByName, CallByValue -> sim_bn_bv t
    | CallByValue, CallByName -> sim_bv_bn t
    | _ -> print_endline "There is not simulation implemented."; t
