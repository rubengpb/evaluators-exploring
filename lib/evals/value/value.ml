open Core.Syntax
open Eval

let eval_value e t =
  match e with
  | One one_e -> (
    match one_e.style with
      | EvalApply -> (
        match one_e.strategy with
        | CallByValue -> failwith "TODO"
        | CallByName -> failwith "TODO"
        | ApplicativeOrder -> failwith "TODO"
        | NormalOrder -> failwith "TODO"
        | HeadReduction -> failwith "TODO"
        | HeadSpine -> failwith "TODO"
        | StricNormalisation -> failwith "TODO"
        | HybridNormalOrder -> failwith "TODO"
        | HybridApplicativeOrder -> failwith "TODO"
        | AheadMachine -> failwith "TODO"
        | HeadApplicativeOrder -> failwith "TODO"
        | SpineApplicativeOrder -> failwith "TODO"
        | BalancedSpineApplicativeOrder -> failwith "TODO"
        )
      | ReadBack -> failwith "TODO"
      | SmallStep -> failwith "TODO"
  )
  | Gen gen_e -> failwith "TODO"
