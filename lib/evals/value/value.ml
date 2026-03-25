open Core.Syntax
open Eval

let eval_value e t =
  match e with
  | One one_e -> (
    match one_e.style with
      | EvalApply -> (
        match one_e.strategy with
        | CallByValue -> TPure (Lv_bv.bv t)
        | CallByName -> TPure (Lv_bn.bn t)
        | ApplicativeOrder -> TPure (Lv_ao.ao t)
        | NormalOrder -> TPure (Lv_no.no t)
        | HeadReduction -> TPure (Lv_hr.hr t)
        | HeadSpine -> TPure (Lv_he.he t)
        | StricNormalisation -> TPure (Lv_sn.sn t)
        | HybridNormalOrder -> TPure (Lv_hn.hn t)
        | HybridApplicativeOrder -> TPure (Lv_ha.ha t)
        | AheadMachine -> TPure (Lv_am.am t)
        | HeadApplicativeOrder -> TPure (Lv_ho.ho t)
        | SpineApplicativeOrder -> TPure (Lv_so.so t)
        | BalancedSpineApplicativeOrder -> TPure (Lv_bs.bs t)
        )
      | ReadBack -> failwith "TODO"
      | SmallStep -> failwith "TODO"
  )
  | Gen gen_e -> failwith "TODO"
