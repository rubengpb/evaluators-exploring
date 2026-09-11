open Eval
open Core.Syntax

let evalapplyzipper_of_short_string = function
  | "id" -> fun x -> x
  | "bv" -> Bv.bv
  | "bn" -> Bn.bn
  | "ao" -> Ao.ao
  | "no" -> No.no
  | "hr" -> Hr.hr
  | "he" -> He.he
  | "sn" -> Sn.sn
  | "hn" -> Hn.hn
  | "ha" -> Ha.ha
  | "am" -> Am.am
  | "ho" -> Ho.ho
  | "so" -> So.so
  | "bs" -> Bs.bs
  | s -> failwith @@ "ERROR: wrong short string: " ^ s

let eval_apply_zipper str t =
  match str with
    | Gen params -> (
      match List.map evalapplyzipper_of_short_string params with
        | [la; op1; ar1; op2; ar2] -> fst @@ Gen.gen la op1 ar1 op2 ar2 (t, CTop)
        | _ -> failwith "ERROR: incorrect number of params in Pure Gen EvalApplyZipper"
      )
    | One name -> (
      match name with
      | CallByValue -> fst @@ Bv.bv (t, CTop)
      | CallByName -> fst @@ Bn.bn (t, CTop)
      | ApplicativeOrder -> fst @@ Ao.ao (t, CTop)
      | NormalOrder -> fst @@ No.no (t, CTop)
      | HeadReduction -> fst @@ Hr.hr (t, CTop)
      | HeadSpine -> fst @@ He.he (t, CTop)
      | StrictNormalisation -> fst @@ Sn.sn (t, CTop)
      | HybridNormalOrder -> fst @@ Hn.hn (t, CTop)
      | HybridApplicativeOrder -> fst @@ Ha.ha (t, CTop)
      | AheadMachine -> fst @@ Am.am (t, CTop)
      | HeadApplicativeOrder -> fst @@ Ho.ho (t, CTop)
      | SpineApplicativeOrder -> fst @@ So.so (t, CTop)
      | BalancedSpineApplicativeOrder -> fst @@ Bs.bs (t, CTop)
    )
