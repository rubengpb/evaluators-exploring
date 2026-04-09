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
        | [la; op1; ar1; op2; ar2] -> fst @@ Gen.gen la op1 ar1 op2 ar2 (t, Top)
        | _ -> failwith "ERROR: incorrect number of params in Pure Gen EvalApplyZipper"
      )
    | One name -> (
      match name with
      | CallByValue -> fst @@ Bv.bv (t, Top)
      | CallByName -> fst @@ Bn.bn (t, Top)
      | ApplicativeOrder -> fst @@ Ao.ao (t, Top)
      | NormalOrder -> fst @@ No.no (t, Top)
      | HeadReduction -> fst @@ Hr.hr (t, Top)
      | HeadSpine -> fst @@ He.he (t, Top)
      | StricNormalisation -> fst @@ Sn.sn (t, Top)
      | HybridNormalOrder -> fst @@ Hn.hn (t, Top)
      | HybridApplicativeOrder -> fst @@ Ha.ha (t, Top)
      | AheadMachine -> fst @@ Am.am (t, Top)
      | HeadApplicativeOrder -> fst @@ Ho.ho (t, Top)
      | SpineApplicativeOrder -> fst @@ So.so (t, Top)
      | BalancedSpineApplicativeOrder -> fst @@ Bs.bs (t, Top)
    )
