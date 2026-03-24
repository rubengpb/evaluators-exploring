open Core.Utils
open Eval
open Printer

let eval_ss str t =
  match str with
    | CallByValue -> Ss_bv.bv t
    | CallByName -> Ss_bn.bn t
    | ApplicativeOrder -> Ss_ao.ao t
    | NormalOrder -> Ss_no.no t
    | HeadReduction -> Ss_hr.hr t
    | HeadSpine -> Ss_he.he t
    | StricNormalisation -> Ss_sn.sn t
    | HybridNormalOrder -> Ss_hn.hn t
    | HybridApplicativeOrder -> Ss_ha.ha t
    | AheadMachine -> Ss_am.am t
    | HeadApplicativeOrder -> Ss_ho.ho t
    | SpineApplicativeOrder -> Ss_so.so t
    | BalancedSpineApplicativeOrder -> Ss_bs.bs t
