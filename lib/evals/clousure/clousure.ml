open Core.Syntax
open Eval

let eval_clou e t =
  match e with
    | Gen _ -> failwith "TODO"
    | One eval -> (
      match eval.style, eval.strategy with
        | Apply, CallByValue -> TClousure (Cl_bv.eval_cl_bv [] t)
        | Apply, CallByName -> TClousure (Cl_bn.eval_cl_bn [] t)
        | Apply, ApplicativeOrder -> TClousure (Cl_ao.eval_cl_ao [] t)
        | Apply, NormalOrder -> TClousure (Cl_no.eval_cl_no [] t)
        | _ -> failwith "TODO"
    )
