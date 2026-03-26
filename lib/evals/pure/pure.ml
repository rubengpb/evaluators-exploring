open Core.Syntax
open Eval
open Core.Forms
open Gen
open Gen_rb

let evalapply_of_short_string = function
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
  | s -> failwith @@ "ERROR: wrong short string: " ^ s

let evalreadback_of_short_string = function
  | "id" -> fun x -> x
  | "rn" -> Rbno.rn
  | "bodies" -> Rbbv.bodies
  | "bodies2" -> Rbam.bodies2
  | "bodies3" -> Rbun.bodies3
  | "args" -> Rbbn.args
  | "bv" -> Bv.eval_bv
  | "bn" -> Bn.eval_bn
  | "he" -> He.eval_he
  | s -> failwith @@ "ERROR: wrong short string: " ^ s

let eval_pure e t =
  match e with
  | One one_e -> (
    match one_e.style with
      | EvalApply -> (
        match one_e.strategy with
        | CallByValue -> TPure (fst (Bv.eval_bv_zipp (t, Top)))
        | CallByName -> TPure (fst (Bn.eval_bn_zipp (t, Top)))
        | ApplicativeOrder -> TPure (fst (Ao.eval_ao_zipp (t, Top)))
        | NormalOrder -> TPure (fst (No.eval_no_zipp (t, Top)))
        | HeadReduction -> TPure (fst (Hr.eval_hr_zipp (t, Top)))
        | HeadSpine -> TPure (fst (He.eval_he_zipp (t, Top)))
        | StricNormalisation -> TPure (fst (Sn.eval_sn_zipp (t, Top)))
        | HybridNormalOrder -> TPure (fst (Hn.eval_hn_zipp (t, Top)))
        | HybridApplicativeOrder -> TPure (fst (Ha.eval_ha_zipp (t, Top)))
        | AheadMachine -> TPure (fst (Am.eval_am_zipp (t, Top)))
        | HeadApplicativeOrder -> TPure (fst (Ho.eval_ho_zipp (t, Top)))
        | SpineApplicativeOrder -> TPure (fst (So.eval_so_zipp (t, Top)))
        | BalancedSpineApplicativeOrder -> TPure (fst (Bs.eval_bs_zipp (t, Top)))
      )
      | ReadBack -> (
        match one_e.strategy with
        | CallByValue -> TPure (Rbbv.eval_rbbv t)
        | CallByName -> TPure (Rbbn.eval_rbbn t)
        | NormalOrder -> TPure (Rbno.eval_rbno t)
        | AheadMachine -> TPure (Rbam.eval_rbam t)
        | _ -> print_endline "Running unnamed evaluator"; TPure (Rbun.eval_rbun t))
      | SmallStep -> TPure (Eval_ss.eval_ss one_e.strategy t)
  )
  | Gen gen_e -> (
    match gen_e.style with
      | EvalApply ->
        let params = List.map evalapply_of_short_string gen_e.params in (
        (* let f = List.fold_left (fun acc_fn arg -> acc_fn arg) gen params in f t *)
        match params with
          | [la; op1; ar1; op2; ar2] -> TPure (gen la op1 ar1 op2 ar2 t)
          | _ -> failwith "ERROR: Incorrect number of params in gen eval."
        )
      | ReadBack ->
        let params = List.map evalreadback_of_short_string gen_e.params in (
        (* let f = List.fold_left (fun acc_fn arg -> acc_fn arg) gen params in f t *)
        match params with
          | [la_1; la_2; ar2_1; ar2_2] ->
            TPure (gen_rb (Fun.compose la_1 la_2) (Fun.compose ar2_1 ar2_2) t)
          | _ -> failwith "ERROR: Incorrect number of params in gen eval."
        )
      | SmallStep -> failwith "TODO"
    )
