open Core.Syntax
open Core.Utils
open Eval
open Main_eval

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
    | NormalOrder, StrictNormalisation -> App(sim_no_sn t, id)
    | StrictNormalisation, NormalOrder -> App(sim_sn_no t, id)
    | _ -> print_endline "There is not simulation implemented."; t

let rec inverse_simulation_ptransform sim ev t =
  let id = Abs("x", Var "x") in
  match sim.guest, sim.host with
    | str1, str2 when str1 = str2 -> t
    | CallByName, CallByValue -> t
    | CallByValue, CallByName -> t
    | NormalOrder, StrictNormalisation -> (
      match t with
        | App(m, Abs(v, Var vs)) when v = vs -> m
        | Abs(x, b) -> (
          let b' = eval ev (App(b, id)) in
            match b' with
              | TPure b -> Abs(x, inverse_simulation_ptransform sim ev b)
              | _ -> failwith "Wrong value to inverse")
        | App(Var x, neu) -> (
          let n' = eval ev (App(neu, id)) in
            match n' with
              | TPure n -> App(Var x, inverse_simulation_ptransform sim ev n)
              | _ -> failwith "Wrong value to inverse")
        | _ -> failwith "Wrong value to inverse"
      )
    | StrictNormalisation, NormalOrder -> (
      match t with
        | App(m, Abs(v, Var vs)) when v = vs -> m
        | Var _ as v -> v
        | Abs(x,b) -> (
          let b' = eval ev (App(b, id)) in
            match b' with
              | TPure b -> Abs(x, inverse_simulation_ptransform sim ev b)
              | _ -> failwith "Wrong value to inverse")
      (* | t -> t *)
        (* | App(Var x, neu) -> ( *)
        (*       let n' = eval ev (App(neu, id)) in *)
        (*         match n' with *)
        (*           | TPure n -> App(Var x, inverse_simulation_ptransform sim ev n) *)
        (*           | _ -> failwith "Wrong value to inverse") *)
        | App(m, n) ->
          let m' = inverse_simulation_ptransform sim ev m in
          let n' = eval ev (App(n, id)) in
            match n' with
              | TPure n -> App(m', inverse_simulation_ptransform sim ev n)
              | _ -> failwith "Wrong value to inverse"
      )
    | _ -> failwith "TODO"

let inverse_simulation_transform sim ev = function
  | TPure t -> TPure (inverse_simulation_ptransform sim ev t)
  | _ -> failwith "Error: No implemented simulation for other substitution manager"
