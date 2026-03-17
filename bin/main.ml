open Core.Syntax
open Core.Utils
open Evals.Eval
open Evals.Main_eval

let main_term = term_of_string "(((\\m.\\n.\\s.\\z. (m s (n s z)) ) 3 ) 2)"


let () =
  print_endline @@ "Evaluating: " ^ string_of_pterm main_term;
  let t_bv = eval (One { language = Pure; strategy = CallByValue; style = Apply; }) main_term in
    print_endline @@ "BV: " ^ string_of_pterm t_bv;
  let t_bn = eval (One { language = Pure; strategy = CallByName; style = Apply; }) main_term in
    print_endline @@ "BN: " ^ string_of_pterm t_bn;
  let t_ao = eval (One { language = Pure; strategy = ApplicativeOrder; style = Apply; }) main_term in
    print_endline @@ "AO: " ^ string_of_pterm t_ao;
  let t_no = eval (One { language = Pure; strategy = NormalOrder; style = Apply; }) main_term in
    print_endline @@ "NO: " ^ string_of_pterm t_no;
  let t_hr = eval (One { language = Pure; strategy = HeadReduction; style = Apply; }) main_term in
    print_endline @@ "HR: " ^ string_of_pterm t_hr;
  let t_he = eval (One { language = Pure; strategy = HeadSpine; style = Apply; }) main_term in
    print_endline @@ "HE: " ^ string_of_pterm t_he;
  let t_sn = eval (One { language = Pure; strategy = StricNormalisation; style = Apply; }) main_term in
    print_endline @@ "SN: " ^ string_of_pterm t_sn;
  let t_hn = eval (One { language = Pure; strategy = HybridNormalOrder; style = Apply; }) main_term in
    print_endline @@ "HN: " ^ string_of_pterm t_hn;
  let t_ha = eval (One { language = Pure; strategy = HeadApplicativeOrder; style = Apply; }) main_term in
    print_endline @@ "HA: " ^ string_of_pterm t_ha;
  let t_am = eval (One { language = Pure; strategy = AheadMachine; style = Apply; }) main_term in
    print_endline @@ "AM: " ^ string_of_pterm t_am;
  let t_ho = eval (One { language = Pure; strategy = HybridNormalOrder; style = Apply; }) main_term in
    print_endline @@ "HO: " ^ string_of_pterm t_ho;
  let t_so = eval (One { language = Pure; strategy = SpineApplicativeOrder; style = Apply; }) main_term in
    print_endline @@ "SO: " ^ string_of_pterm t_so;
  let t_bs = eval (One { language = Pure; strategy = BalancedSpineApplicativeOrder; style = Apply; }) main_term in
    print_endline @@ "BS: " ^ string_of_pterm t_bs;
  let eval_gen = eval (Gen {language = Pure; style = Apply; params = ["no";"bn";"id";"no";"no"]}) in
  let t_gen = eval_gen main_term in
    print_endline @@ "GEN no bn id no no: " ^ string_of_pterm t_gen;
  let eval_gen = eval (Gen {language = Pure; style = Apply; params = ["id";"bv";"bv";"id";"bv"]}) in
  let t_gen = eval_gen main_term in
    print_endline @@ "GEN id bv bv id bv: " ^ string_of_pterm t_gen;
  print_endline "Read-Back Evaluators:";
  let t_bs = eval (One {style = ReadBack; language = Pure; strategy = CallByValue; }) main_term in
    print_endline @@ "RBBV: " ^ string_of_pterm t_bs;
  let t_bs = eval (One {style = ReadBack; language = Pure; strategy = CallByName; }) main_term in
    print_endline @@ "RBBN: " ^ string_of_pterm t_bs;
  let t_bs = eval (One {style = ReadBack; language = Pure; strategy = NormalOrder; }) main_term in
    print_endline @@ "RBNO: " ^ string_of_pterm t_bs;
  let t_bs = eval (One {style = ReadBack; language = Pure; strategy = AheadMachine; }) main_term in
    print_endline @@ "RBAM: " ^ string_of_pterm t_bs;
  let t_bs = eval (One {style = ReadBack; language = Pure; strategy = SpineApplicativeOrder; }) main_term in
    print_endline @@ "RBUN: " ^ string_of_pterm t_bs;
  print_endline "Small Step Evaluators:";
  print_endline "== Normal Order ==";
  let t_ssno = eval (One {style = SmallStep; language = Pure; strategy = NormalOrder;}) main_term in
    print_endline @@ string_of_pterm t_ssno;
  print_endline "== Applicative Order ==";
  let t_ssno = eval (One {style = SmallStep; language = Pure; strategy = ApplicativeOrder;}) main_term in
    print_endline @@ string_of_pterm t_ssno;
