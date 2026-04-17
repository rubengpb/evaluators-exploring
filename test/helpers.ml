open Core
open Syntax
open Utils
open Church_numerals

let id = Abs("x", Var "x")
let const = Abs("x", Abs ("y", Var "x"))
let delta = Abs("x", App (Var "x", Var "x"))
let omega = App(delta, delta)
let add = term_of_string "\\m.\\n.\\s.\\z. m s (n s z)"
let prod = term_of_string "\\m.\\n.\\s. m (n s)"
let pow = term_of_string "\\m.\\n.n m"
let isZero = term_of_string "\\n. n (\\x. (\\x.\\y. y)) (\\x.\\y. x)"

let basic_redex = App(id, Var "y")
let basic_neu = App(Var "x", App(id, Var "y"))
let basic_neu_without_redex = App(App(Var "x", id), Var "y")
let redex_in_abs = Abs("x", App(id, Var "y"))
let redex_in_abs_in_app = App(redex_in_abs, basic_redex)
let redex_in_abs_in_app_neu = App(redex_in_abs, basic_neu)
let neu_neu = App(basic_neu, basic_neu)
let not_diver = App(App(const, Var "y"), omega)
let add_100_100 = App(App(add, pterm_of_int 100), pterm_of_int 100)
let prod_10_12 = App(App(prod, pterm_of_int 10), pterm_of_int 12)
let pow_3_4 = App(App(pow, pterm_of_int 3), pterm_of_int 4)
let isZero_0 = App(isZero, pterm_of_int 0)
let isZero_100 = App(isZero, pterm_of_int 0)
