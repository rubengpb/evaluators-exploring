open Alcotest
open Core.Syntax
open Core.Utils
open Core.Church_numerals
open Evals.Eval
open Evals.Main_eval
open Helpers

let ea = eval { language = Pure; subst = Subst; style = EvalApply; strategy = One ApplicativeOrder; }
let ss = eval { language = Pure; subst = Subst; style = SmallStep; strategy = One ApplicativeOrder; }
let zi = eval { language = Pure; subst = Subst; style = EvalApply; strategy = One ApplicativeOrder; }
let gen = eval { language = Pure; subst = Subst; style = EvalApply; strategy = Gen ["ao"; "ao"; "ao"; "id"; "ao"]; }

let basic_redex () =
  let r_ea = ea basic_redex in
  let r_ss = ss basic_redex in
  let r_zi = zi basic_redex in
  let r_gen = gen basic_redex in
  check string "Eval basic redex" "y" (string_of_term r_ea);
  check string "Eval basic redex" "y" (string_of_term r_ss);
  check string "Eval basic redex" "y" (string_of_term r_zi);
  check string "Eval basic redex" "y" (string_of_term r_gen)

let basic_neu () =
  let r_ea = ea basic_neu in
  let r_ss = ss basic_neu in
  let r_zi = zi basic_neu in
  let r_gen = gen basic_neu in
  let result = "x y" in
  check string "Eval basic neu" result (string_of_term r_ea);
  check string "Eval basic neu" result (string_of_term r_ss);
  check string "Eval basic neu" result (string_of_term r_zi);
  check string "Eval basic neu" result (string_of_term r_gen)

let basic_neu_without_redex () =
  let r_ea = ea basic_neu_without_redex in
  let r_ss = ss basic_neu_without_redex in
  let r_zi = zi basic_neu_without_redex in
  let r_gen = gen basic_neu_without_redex in
  let result = "x (\\x.x) y" in
  check string "Eval basic neu without redex" result (string_of_term r_ea);
  check string "Eval basic neu without redex" result (string_of_term r_ss);
  check string "Eval basic neu without redex" result (string_of_term r_zi);
  check string "Eval basic neu without redex" result (string_of_term r_gen)

let redex_in_abs () =
  let r_ea = ea redex_in_abs in
  let r_ss = ss redex_in_abs in
  let r_zi = zi redex_in_abs in
  let r_gen = gen redex_in_abs in
  let result = "\\x.y" in
  check string "Redex inside abstraction" result (string_of_term r_ea);
  check string "Redex inside abstraction" result (string_of_term r_ss);
  check string "Redex inside abstraction" result (string_of_term r_zi);
  check string "Redex inside abstraction" result (string_of_term r_gen)

let redex_in_abs_in_app () =
  let r_ea = ea redex_in_abs_in_app in
  let r_ss = ss redex_in_abs_in_app in
  let r_zi = zi redex_in_abs_in_app in
  let r_gen = gen redex_in_abs_in_app in
  let result = "y" in
  check string "Redex in operator and operand" result (string_of_term r_ea);
  check string "Redex in operator and operand" result (string_of_term r_ss);
  check string "Redex in operator and operand" result (string_of_term r_zi);
  check string "Redex in operator and operand" result (string_of_term r_gen)

let redex_in_abs_in_app_neu () =
  let r_ea = ea redex_in_abs_in_app_neu in
  let r_ss = ss redex_in_abs_in_app_neu in
  let r_zi = zi redex_in_abs_in_app_neu in
  let r_gen = gen redex_in_abs_in_app_neu in
  let result = "y" in
  check string "Neu in operand" result (string_of_term r_ea);
  check string "Neu in operand" result (string_of_term r_ss);
  check string "Neu in operand" result (string_of_term r_zi);
  check string "Neu in operand" result (string_of_term r_gen)

let neu_neu () =
  let r_ea = ea neu_neu in
  let r_ss = ss neu_neu in
  let r_zi = zi neu_neu in
  let r_gen = gen neu_neu in
  let result = "x y (x y)" in
  check string "App of two neu" result (string_of_term r_ea);
  check string "App of two neu" result (string_of_term r_ss);
  check string "App of two neu" result (string_of_term r_zi);
  check string "App of two neu" result (string_of_term r_gen)

let extract_pure : term -> pterm = function
  | TPure x -> x
  | _ -> Alcotest.fail "Expected TPure"

let add_100_100 () =
  let r_ea = extract_pure @@ ea add_100_100 in
  let r_ss = extract_pure @@ ss add_100_100 in
  let r_zi = extract_pure  @@ zi add_100_100 in
  let r_gen = extract_pure @@ gen add_100_100 in
  let result = pterm_of_int 200 in
  check bool "Addition EvalApply" true (alpha_equiv result r_ea);
  check bool "Addition SmallStep" true (alpha_equiv result r_ss);
  check bool "Addition Zipper" true (alpha_equiv result r_zi);
  check bool "Addition Gen" true (alpha_equiv result r_gen)

let prod_10_12 () =
  let r_ea = extract_pure @@ ea prod_10_12 in
  let r_ss = extract_pure @@ ss prod_10_12 in
  let r_zi = extract_pure  @@ zi prod_10_12 in
  let r_gen = extract_pure @@ gen prod_10_12 in
  let result = pterm_of_int 120 in
  check bool "Prod" true (alpha_equiv result r_ea);
  check bool "Prod" true (alpha_equiv result r_ss);
  check bool "Prod" true (alpha_equiv result r_zi);
  check bool "Prod" true (alpha_equiv result r_gen)

let pow_3_4 () =
  let r_ea = extract_pure @@ ea pow_3_4 in
  let r_ss = extract_pure @@ ss pow_3_4 in
  let r_zi = extract_pure  @@ zi pow_3_4 in
  let r_gen = extract_pure @@ gen pow_3_4 in
  let result = pterm_of_int 81 in
  check bool "Pow" true (alpha_equiv result r_ea);
  check bool "Pow" true (alpha_equiv result r_ss);
  check bool "Pow" true (alpha_equiv result r_zi);
  check bool "Pow" true (alpha_equiv result r_gen)

let () =
  run "eval" [
    ("beta", [
      test_case "Basic redex" `Quick basic_redex;
      test_case "Basic neu" `Quick basic_neu;
      test_case "Basic neu - no redex" `Quick basic_neu_without_redex;
      test_case "Redex inside abstraction" `Quick redex_in_abs;
      test_case "Redex in operator and operand" `Quick redex_in_abs_in_app;
      test_case "Neu in operand" `Quick redex_in_abs_in_app_neu;
      test_case "100 + 100" `Quick add_100_100;
      test_case "10 * 12" `Quick prod_10_12;
      test_case "3 ** 4" `Quick pow_3_4;
    ]);
  ]
