open Core.Syntax
open Eval
open Core.Forms
open Gen

let eval_of_short_string = function
  | "id" -> fun x -> x
  | "bv" -> Bv.eval_bv
  | "bn" -> Bn.eval_bn
  | "ao" -> Ao.eval_ao
  | "no" -> No.eval_no
  | "hr" -> Hr.eval_hr
  | "he" -> He.eval_he
  | "sn" -> Sn.eval_sn
  | "hn" -> Hn.eval_hn
  | "ha" -> Ha.eval_ha
  | "am" -> Am.eval_am
  | "ho" -> Ho.eval_ho
  | "so" -> So.eval_so
  | "bs" -> Bs.eval_bs
  | _ -> failwith "ERROR: wrong short string"

let eval_pure e t =
  match e with
  | One one_e -> (
    match one_e.style with
      | Apply -> (
        match one_e.strategy with
        | CallByValue -> TPure (Bv.eval_bv t)
        | CallByName -> TPure (Bn.eval_bn t)
        | ApplicativeOrder -> TPure (Ao.eval_ao t)
        | NormalOrder -> TPure (No.eval_no t)
        | HeadReduction -> TPure (Hr.eval_hr t)
        | HeadSpine -> TPure (He.eval_he t)
        | StricNormalisation -> TPure (Sn.eval_sn t)
        | HybridNormalOrder -> TPure (Hn.eval_hn t)
        | HybridApplicativeOrder -> TPure (Ha.eval_ha t)
        | AheadMachine -> TPure (Am.eval_am t)
        | HeadApplicativeOrder -> TPure (Ho.eval_ho t)
        | SpineApplicativeOrder -> TPure (So.eval_so t)
        | BalancedSpineApplicativeOrder -> TPure (Bs.eval_bs t))
      | ReadBack -> (
        match one_e.strategy with
        | CallByValue -> TPure (Rbbv.eval_rbbv t)
        | CallByName -> TPure (Rbbn.eval_rbbn t)
        | NormalOrder -> TPure (Rbno.eval_rbno t)
        | AheadMachine -> TPure (Rbam.eval_rbam t)
        | _ -> print_endline "Running unnamed evaluator"; TPure (Rbun.eval_rbun t))
      | SmallStep -> (
        match one_e.strategy with
        | NormalOrder -> TPure (Ss.eval_ss is_nf "outermost" Ssno.step_left_outer t)
        | ApplicativeOrder -> TPure (Ss.eval_ss is_nf "innermost" Ssao.step_left_inner t)
        | _ -> failwith "TODO"))
  | Gen gen_e -> (
    match gen_e.style with
      | Apply ->
        let params = List.map eval_of_short_string gen_e.params in (
        (* let f = List.fold_left (fun acc_fn arg -> acc_fn arg) gen params in f t *)
        match params with
          | [la; op1; ar1; op2; ar2] -> TPure (gen la op1 ar1 op2 ar2 t)
          | _ -> failwith "ERROR: Incorrect number of params in gen eval."
        )
      | ReadBack -> failwith "TODO"
      | SmallStep -> failwith "TODO"
    )
