open Eval

let eval strat t =
  match strat with
  | CallByValue -> Bv.eval_bv t
  | CallByName -> Bn.eval_bn t
  | ApplicativeOrder -> Ao.eval_ao t
  | Normal -> No.eval_no t
  | HeadReduction -> Hr.eval_hr t
  | HeadSpine -> He.eval_he t
  | StricNormalisation -> Sn.eval_sn t
  | HybridNormalOrder -> Hn.eval_hn t
  | HybridApplicativeOrder -> Ha.eval_ha t
  | AheadMachine -> Am.eval_am t
  | HeadApplicativeOrder -> Ho.eval_ho t
  | SpineApplicativeOrder -> So.eval_so t
  | BalancedSpineApplicativeOrder -> Bs.eval_bs t
