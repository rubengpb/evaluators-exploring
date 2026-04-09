open Core.Syntax
open Core.Utils
open Eval

let rec sim_bn_bv t =
  let id = Abs("+", Var "+") in
  match t with
    | Var x as v -> v
    | Abs(x, b) -> Abs(x, subst (App(Var x, id)) x @@ sim_bn_bv b)
    | App(m, n) -> App(sim_bn_bv m, (Abs("*", App(Var "*", sim_bn_bv n))))

let simulation_transform sim t =
  match sim.guest, sim.host with
    | CallByName, CallByValue -> sim_bn_bv t
    | _ -> print_endline "There is not simulation implemented."; t
