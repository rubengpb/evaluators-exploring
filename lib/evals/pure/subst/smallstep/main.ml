open Eval
open Core.Forms
open Core.Syntax

let evalapply_of_short_string : string -> ((pterm -> pterm) * (pterm -> bool)) = function
  | "id" -> ((fun x -> x), fun _ -> true)
  | "bv" -> (Bv.bv, is_wnf)
  | "bn" -> (Bn.bn, is_whnf)
  | "ao" -> (Ao.ao, is_nf)
  | "no" -> (No.no, is_nf)
  | "hr" -> (Hr.hr, is_hnf)
  | "he" -> (He.he, is_hnf)
  | "sn" -> (Sn.sn, is_nf)
  | "hn" -> (Hn.hn, is_nf)
  | "ha" -> (Ha.ha, is_nf)
  | "am" -> (Am.am, is_vhnf)
  | "ho" -> (Ho.ho, is_hnf)
  | "so" -> (So.so, is_nf)
  | "bs" -> (Bs.bs, is_nf)
  | s -> failwith @@ "ERROR: wrong short string: " ^ s


let manage_form = function
    | [la; op1] ->
      if la = "id" then snd (evalapply_of_short_string op1)
      else snd (evalapply_of_short_string la)
    | _ -> failwith "ERROR: incorrect number of params in Pure Gen SmallStep"

let eval_smallstep str t =
  match str with
    | Gen params -> (
      let is_gen = manage_form (List.take 2 params) in
      match List.map evalapply_of_short_string params with
        | [la; op1; ar1; op2; ar2] -> Gen.gen is_gen la op1 ar1 op2 ar2 t
        | _ -> failwith "ERROR: incorrect number of params in Pure Gen SmallStep"
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
