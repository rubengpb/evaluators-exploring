open Core.Syntax
open Eval

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
        | [la; op1; ar1; op2; ar2] -> fst @@ Gen.gen la op1 ar1 op2 ar2 (t, DBTop)
        | _ -> failwith "ERROR: incorrect number of params in Pure Gen EvalApplyZipper"
      )
    | One name -> (
      match name with
      | CallByValue -> fst @@ Bv.bv (t, DBTop)
      | CallByName -> fst @@ Bn.bn (t, DBTop)
      | ApplicativeOrder -> fst @@ Ao.ao (t, DBTop)
      | NormalOrder -> fst @@ No.no (t, DBTop)
      | HeadReduction -> fst @@ Hr.hr (t, DBTop)
      | HeadSpine -> fst @@ He.he (t, DBTop)
      | StricNormalisation -> fst @@ Sn.sn (t, DBTop)
      | HybridNormalOrder -> fst @@ Hn.hn (t, DBTop)
      | HybridApplicativeOrder -> fst @@ Ha.ha (t, DBTop)
      | AheadMachine -> fst @@ Am.am (t, DBTop)
      | HeadApplicativeOrder -> fst @@ Ho.ho (t, DBTop)
      | SpineApplicativeOrder -> fst @@ So.so (t, DBTop)
      | BalancedSpineApplicativeOrder -> fst @@ Bs.bs (t, DBTop)
    )
