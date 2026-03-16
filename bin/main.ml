open Core.Syntax
open Core.Utils
open Evals.Eval
open Evals.Main_eval

let main_term = term_of_string "(((\\m.\\n.\\s.\\z. (m s (n s z)) ) 3 ) 2)"


let () =
  print_endline @@ "Evaluating: " ^ string_of_term main_term;
  let t_bv = eval (One { strategy = CallByValue; style = Apply; }) main_term in
    print_endline @@ "BV: " ^ string_of_term t_bv;
  let t_bn = eval (One { strategy = CallByName; style = Apply; }) main_term in
    print_endline @@ "BN: " ^ string_of_term t_bn;
  let t_ao = eval (One { strategy = ApplicativeOrder; style = Apply; }) main_term in
    print_endline @@ "AO: " ^ string_of_term t_ao;
  let t_no = eval (One { strategy = NormalOrder; style = Apply; }) main_term in
    print_endline @@ "NO: " ^ string_of_term t_no;
  let t_hr = eval (One { strategy = HeadReduction; style = Apply; }) main_term in
    print_endline @@ "HR: " ^ string_of_term t_hr;
  let t_he = eval (One { strategy = HeadSpine; style = Apply; }) main_term in
    print_endline @@ "HE: " ^ string_of_term t_he;
  let t_sn = eval (One { strategy = StricNormalisation; style = Apply; }) main_term in
    print_endline @@ "SN: " ^ string_of_term t_sn;
  let t_hn = eval (One { strategy = HybridNormalOrder; style = Apply; }) main_term in
    print_endline @@ "HN: " ^ string_of_term t_hn;
  let t_ha = eval (One { strategy = HeadApplicativeOrder; style = Apply; }) main_term in
    print_endline @@ "HA: " ^ string_of_term t_ha;
  let t_am = eval (One { strategy = AheadMachine; style = Apply; }) main_term in
    print_endline @@ "AM: " ^ string_of_term t_am;
  let t_ho = eval (One { strategy = HybridNormalOrder; style = Apply; }) main_term in
    print_endline @@ "HO: " ^ string_of_term t_ho;
  let t_so = eval (One { strategy = SpineApplicativeOrder; style = Apply; }) main_term in
    print_endline @@ "SO: " ^ string_of_term t_so;
  let t_bs = eval (One { strategy = BalancedSpineApplicativeOrder; style = Apply; }) main_term in
    print_endline @@ "BS: " ^ string_of_term t_bs;
  let eval_gen = eval (Gen {style = Apply; params = ["no";"bn";"id";"no";"no"]}) in
  let t_gen = eval_gen main_term in
    print_endline @@ "GEN no bn id no no: " ^ string_of_term t_gen;
  let eval_gen = eval (Gen {style = Apply; params = ["id";"bv";"bv";"id";"bv"]}) in
  let t_gen = eval_gen main_term in
    print_endline @@ "GEN id bv bv id bv: " ^ string_of_term t_gen;
  print_endline "Read-Back Evaluators:";
  let t_bs = eval (One {style = ReadBack; strategy = CallByValue; }) main_term in
    print_endline @@ "RBBV: " ^ string_of_term t_bs;
  let t_bs = eval (One {style = ReadBack; strategy = CallByName; }) main_term in
    print_endline @@ "RBBN: " ^ string_of_term t_bs;
  let t_bs = eval (One {style = ReadBack; strategy = NormalOrder; }) main_term in
    print_endline @@ "RBNO: " ^ string_of_term t_bs;
  let t_bs = eval (One {style = ReadBack; strategy = AheadMachine; }) main_term in
    print_endline @@ "RBAM: " ^ string_of_term t_bs;
  let t_bs = eval (One {style = ReadBack; strategy = SpineApplicativeOrder; }) main_term in
    print_endline @@ "RBUN: " ^ string_of_term t_bs;
  print_endline "Small Step Evaluators:";
  print_endline "== Normal Order ==";
  let t_ssno = eval (One {style = SmallStep; strategy = NormalOrder;}) main_term in
    print_endline @@ string_of_term t_ssno;
  print_endline "== Applicative Order ==";
  let t_ssno = eval (One {style = SmallStep; strategy = ApplicativeOrder;}) main_term in
    print_endline @@ string_of_term t_ssno;
