open Core.Syntax
open Eval

let eval_clou e t =
  match e with
    | Gen _ -> failwith "TODO"
    | One eval -> (
      match eval.style, eval.strategy with
        | EvalApply, CallByValue -> TClousure (Cl_bv.eval_cl_bv [] t)
        | EvalApply, CallByName -> TClousure (Cl_bn.eval_cl_bn (Clou (t,[])))
        | EvalApply, ApplicativeOrder -> TClousure (Cl_ao.eval_cl_ao (Clou (t, [])))
        | EvalApply, NormalOrder -> TClousure (Cl_no.eval_cl_no (Clou (t, [])))
        | _ -> failwith "TODO"
    )
