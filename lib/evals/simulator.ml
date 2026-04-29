open Core.Syntax
open Core.Utils
open Eval

let rec sim_bn_bv t =
  match t with
    (* | Var x as v -> Abs("A", App(Var "A", v)) *)
    | Var x as v -> v
    | Abs(x, b) ->
      let b' = sim_bn_bv b in
      Abs("A'", App(Var "A'", Abs(x, b')))
    | App(m, n) ->
      let m', n' = sim_bn_bv m, sim_bn_bv n in
      Abs("A'", App(m', (Abs("B'", App(App(Var "B'", n'), Var"A'")))))

let rec sim_bv_bn t =
  match t with
    | Var x as v -> Abs("A", App(Var "A", v))
    | Abs(x, b) ->
      let b' = sim_bv_bn b in
      Abs("A'", App(Var "A'", Abs(x, b')))
    | App(m, n) ->
      let m', n' = sim_bv_bn m, sim_bv_bn n in
      Abs("A'", App(m', Abs("B'", App(n', Abs("C'", App(App(Var "B'", Var "C'"), Var "A'"))))))

let sim_no_sn t = sim_bn_bv t
let sim_sn_no t = sim_bv_bn t

let simulation_transform sim t =
  let id = Abs("x", Var "x") in
  match sim.guest, sim.host with
    | str1, str2 when str1 = str2 -> t
    | CallByName, CallByValue -> App(sim_bn_bv t, id)
    | CallByValue, CallByName -> App(sim_bv_bn t, id)
    | NormalOrder, StricNormalisation -> App(sim_no_sn t, id)
    | StricNormalisation, NormalOrder -> App(sim_sn_no t, id)
    | _ -> print_endline "There is not simulation implemented."; t

let inverse_simulation_transform _ _ t = t
