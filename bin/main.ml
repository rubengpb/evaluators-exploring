open Core.Syntax
open Core.Utils
open Evals.Eval
open Evals.Main_eval

let main_term = term_of_string "(((\\m.\\n.\\s.\\z. (m s (n s z)) ) 3 ) 2)"


let () =
  print_endline @@ "Evaluating: " ^ string_of_term main_term;
  let t_bv = eval CallByValue main_term in
    print_endline @@ "BV: " ^ string_of_term t_bv;
  let t_bn = eval CallByName main_term in
    print_endline @@ "BN: " ^ string_of_term t_bn;
  let t_ao = eval ApplicativeOrder main_term in
    print_endline @@ "AO: " ^ string_of_term t_ao;
  let t_no = eval Normal main_term in
    print_endline @@ "NO: " ^ string_of_term t_no;
  let t_hr = eval HeadReduction main_term in
    print_endline @@ "HR: " ^ string_of_term t_hr;
  let t_he = eval HeadSpine main_term in
    print_endline @@ "HE: " ^ string_of_term t_he;
  let t_sn = eval StricNormalisation main_term in
    print_endline @@ "SN: " ^ string_of_term t_sn;
  let t_hn = eval HybridNormalOrder main_term in
    print_endline @@ "HN: " ^ string_of_term t_hn;
  let t_ha = eval HybridApplicativeOrder main_term in
    print_endline @@ "HA: " ^ string_of_term t_ha;
  let t_am = eval AheadMachine main_term in
    print_endline @@ "AM: " ^ string_of_term t_am;
  let t_ho = eval HeadApplicativeOrder main_term in
    print_endline @@ "HO: " ^ string_of_term t_ho;
  let t_so = eval SpineApplicativeOrder main_term in
    print_endline @@ "SO: " ^ string_of_term t_so;
  let t_bs = eval BalancedSpineApplicativeOrder main_term in
    print_endline @@ "BS: " ^ string_of_term t_bs;
  print_endline "Small Step Evaluators:";
  print_endline "== Normal Order ==";
  let t_ssno = eval SmallStepNormalOrder main_term in
    print_endline @@ string_of_term t_ssno;
  print_endline "== Applicative Order ==";
  let t_ssno = eval SmallStepApplicativeOrder main_term in
    print_endline @@ string_of_term t_ssno;
