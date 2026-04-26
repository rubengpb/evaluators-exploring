open Eval

let evalapply_of_short_string = function
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

let eval_apply str t =
  match str with
    | Gen params -> (
      match List.map evalapply_of_short_string params with
        | [la; op1; ar1; op2; ar2] -> Gen.gen la op1 ar1 op2 ar2 t
        | _ -> failwith "ERROR: incorrect number of params in Pure Gen EvalApply"
      )
    | One name -> (
      match name with
      | CallByValue -> Bv.bv t
      | CallByName -> Bn.bn t
      | ApplicativeOrder -> Ao.ao t
      | NormalOrder -> No.no t
      | HeadReduction -> Hr.hr t
      | HeadSpine -> He.he t
      | StricNormalisation -> Sn.sn t
      | HybridNormalOrder -> Hn.hn t
      | HybridApplicativeOrder -> Ha.ha t
      | AheadMachine -> Am.am t
      | HeadApplicativeOrder -> Ho.ho t
      | SpineApplicativeOrder -> So.so t
      | BalancedSpineApplicativeOrder -> Bs.bs t
    )
