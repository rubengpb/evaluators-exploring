open Core.Syntax
open Core.Utils
open Evals.Eval
open Evals.Main_eval

let main_term = term_of_string "(((\\m.\\n.\\s.\\z. (m s (n s z)) ) 3 ) 2)"


let () =
  print_endline @@ "Evaluating: " ^ string_of_pterm main_term;
  let t_bv = eval { language = Pure; subst = Subst;  strategy = One CallByValue; style = EvalApply; } main_term in
    print_endline @@ "BV: " ^ string_of_term t_bv;
  let t_bn = eval { language = Pure; subst = Subst; strategy = One CallByName; style = EvalApply; } main_term in
    print_endline @@ "BN: " ^ string_of_term t_bn;
  let t_ao = eval  { language = Pure; subst = Subst; strategy = One ApplicativeOrder; style = EvalApply; } main_term in
    print_endline @@ "AO: " ^ string_of_term t_ao;
  let t_no = eval  { language = Pure; subst = Subst; strategy = One NormalOrder; style = EvalApply; } main_term in
    print_endline @@ "NO: " ^ string_of_term t_no;
  let t_hr = eval  { language = Pure; subst = Subst; strategy = One HeadReduction; style = EvalApply; } main_term in
    print_endline @@ "HR: " ^ string_of_term t_hr;
  let t_he = eval  { language = Pure; subst = Subst; strategy = One HeadSpine; style = EvalApply; } main_term in
    print_endline @@ "HE: " ^ string_of_term t_he;
  let t_sn = eval  { language = Pure; subst = Subst; strategy = One StricNormalisation; style = EvalApply; } main_term in
    print_endline @@ "SN: " ^ string_of_term t_sn;
  let t_hn = eval  { language = Pure; subst = Subst; strategy = One HybridNormalOrder; style = EvalApply; } main_term in
    print_endline @@ "HN: " ^ string_of_term t_hn;
  let t_ha = eval  { language = Pure; subst = Subst; strategy = One HeadApplicativeOrder; style = EvalApply; } main_term in
    print_endline @@ "HA: " ^ string_of_term t_ha;
  let t_am = eval  { language = Pure; subst = Subst; strategy = One AheadMachine; style = EvalApply; } main_term in
    print_endline @@ "AM: " ^ string_of_term t_am;
  let t_ho = eval  { language = Pure; subst = Subst; strategy = One HybridNormalOrder; style = EvalApply; } main_term in
    print_endline @@ "HO: " ^ string_of_term t_ho;
  let t_so = eval  { language = Pure; subst = Subst; strategy = One SpineApplicativeOrder; style = EvalApply; } main_term in
    print_endline @@ "SO: " ^ string_of_term t_so;
  let t_bs = eval  { language = Pure; subst = Subst; strategy = One BalancedSpineApplicativeOrder; style = EvalApply; } main_term in
    print_endline @@ "BS: " ^ string_of_term t_bs;
  let eval_gen = eval  {language = Pure; subst = Subst; style = EvalApply; strategy = Gen ["no";"bn";"id";"no";"no"]} in
  let t_gen = eval_gen main_term in
    print_endline @@ "GEN no bn id no no: " ^ string_of_term t_gen;
  let eval_gen = eval ( {language = Pure; subst = Subst; style = EvalApply; strategy = Gen ["id";"bv";"bv";"id";"bv"]}) in
  let t_gen = eval_gen main_term in
    print_endline @@ "GEN id bv bv id bv: " ^ string_of_term t_gen;
  print_endline "Read-Back Evaluators:";
  let t_bs = eval  {style = ReadBack; language = Pure; subst = Subst; strategy = One CallByValue; } main_term in
    print_endline @@ "RBBV: " ^ string_of_term t_bs;
  let t_bs = eval  {style = ReadBack; language = Pure; subst = Subst; strategy = One CallByName; } main_term in
    print_endline @@ "RBBN: " ^ string_of_term t_bs;
  let t_bs = eval  {style = ReadBack; language = Pure; subst = Subst; strategy = One NormalOrder; } main_term in
    print_endline @@ "RBNO: " ^ string_of_term t_bs;
  let t_bs = eval  {style = ReadBack; language = Pure; subst = Subst; strategy = One AheadMachine; } main_term in
    print_endline @@ "RBAM: " ^ string_of_term t_bs;
  let t_bs = eval  {style = ReadBack; language = Pure; subst = Subst; strategy = One SpineApplicativeOrder; } main_term in
    print_endline @@ "RBUN: " ^ string_of_term t_bs;
  print_endline "Small Step Evaluators:";
  print_endline "== Normal Order ==";
  let t_ssno = eval  {style = SmallStep; language = Pure; subst = Subst; strategy = One NormalOrder;} main_term in
    print_endline @@ string_of_term t_ssno;
  print_endline "== Applicative Order ==";
  let t_ssno = eval  {style = SmallStep; language = Pure; subst = Subst; strategy = One ApplicativeOrder;} main_term in
    print_endline @@ string_of_term t_ssno;
