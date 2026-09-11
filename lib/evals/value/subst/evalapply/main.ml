open Eval

let eval_apply str t =
  match str with
    | Gen params ->
      failwith "TODO"
    | One name -> (
      match name with
      | CallByValue -> Bv.bv t
      | CallByName -> Bn.bn t
      | ApplicativeOrder -> Ao.ao t
      | NormalOrder -> No.no t
      | HeadReduction -> Hr.hr t
      | HeadSpine -> He.he t
      | StrictNormalisation -> Sn.sn t
      | HybridNormalOrder -> Hn.hn t
      | HybridApplicativeOrder -> Ha.ha t
      | AheadMachine -> Am.am t
      | HeadApplicativeOrder -> Ho.ho t
      | SpineApplicativeOrder -> So.so t
      | BalancedSpineApplicativeOrder -> Bs.bs t
    )
