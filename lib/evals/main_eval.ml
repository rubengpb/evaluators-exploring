open Eval
open Core.Forms

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
  | SmallStepNormalOrder -> Ss.eval_ss is_nf Ssno.step_left_outer t
  | SmallStepApplicativeOrder -> Ss.eval_ss is_nf Ssao.step_left_inner t
  | ReadBackCallByValue -> Rbbv.eval_rbbv t
  | ReadBackCallByName -> Rbbn.eval_rbbn t
  | ReadBackNormalOrder -> Rbno.eval_rbno t
  | ReadBackAheadMachine -> Rbam.eval_rbam t
  | ReadBackUnnamed -> Rbun.eval_rbun t
