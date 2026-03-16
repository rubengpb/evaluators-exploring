open Eval
open Gen
open Core.Forms

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

let eval e t =
  match e with
  | One one_e -> (
    match one_e.style with
      | Apply -> (
        match one_e.strategy with
        | CallByValue -> Bv.eval_bv t
        | CallByName -> Bn.eval_bn t
        | ApplicativeOrder -> Ao.eval_ao t
        | NormalOrder -> No.eval_no t
        | HeadReduction -> Hr.eval_hr t
        | HeadSpine -> He.eval_he t
        | StricNormalisation -> Sn.eval_sn t
        | HybridNormalOrder -> Hn.eval_hn t
        | HybridApplicativeOrder -> Ha.eval_ha t
        | AheadMachine -> Am.eval_am t
        | HeadApplicativeOrder -> Ho.eval_ho t
        | SpineApplicativeOrder -> So.eval_so t
        | BalancedSpineApplicativeOrder -> Bs.eval_bs t)
      | Clousure -> Eval_cl.eval_cl_pure t
      | ReadBack -> (
        match one_e.strategy with
        | CallByValue -> Rbbv.eval_rbbv t
        | CallByName -> Rbbn.eval_rbbn t
        | NormalOrder -> Rbno.eval_rbno t
        | AheadMachine -> Rbam.eval_rbam t
        | _ -> print_endline "Running unnamed evaluator"; Rbun.eval_rbun t)
      | SmallStep -> (
        match one_e.strategy with
        | NormalOrder -> Ss.eval_ss is_nf "outermost" Ssno.step_left_outer t
        | ApplicativeOrder -> Ss.eval_ss is_nf "innermost" Ssao.step_left_inner t
        | _ -> failwith "TODO"))
  | Gen gen_e -> (
    match gen_e.style with
      | Apply ->
        let params = List.map eval_of_short_string gen_e.params in (
        (* let f = List.fold_left (fun acc_fn arg -> acc_fn arg) gen params in f t *)
        match params with
          | [la; op1; ar1; op2; ar2] -> gen la op1 ar1 op2 ar2 t
          | _ -> failwith "ERROR: Incorrect number of params in gen eval."
        )
      | ReadBack -> failwith "TODO"
      | SmallStep -> failwith "TODO"
      | Clousure -> failwith "TODO"
    )
