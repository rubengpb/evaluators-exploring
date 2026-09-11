type language =
  | Pure
  | Value

type subst =
  | Subst
  | DeBruijn
  | Clousure

type style =
  | EvalApply
  | EvalApplyZipper
  | ReadBack
  | SmallStep

type pstrategy =
  | CallByValue
  | CallByName
  | ApplicativeOrder
  | NormalOrder
  | HeadReduction
  | HeadSpine
  | StrictNormalisation
  | HybridNormalOrder
  | HybridApplicativeOrder
  | AheadMachine
  | HeadApplicativeOrder
  | SpineApplicativeOrder
  | BalancedSpineApplicativeOrder

type strategy =
  | One of pstrategy
  | Gen of string list

type eval = {
  language : language;
  subst : subst;
  style : style;
  strategy : strategy;
}

type simulator = {
  guest : pstrategy;
  host : pstrategy;
}

val string_of_language : language -> string
val language_of_string : string -> language option
val string_of_subst : subst -> string
val subst_of_string : string -> subst option
val string_of_style : style -> string
val style_of_string : string -> style option
val string_of_pstrategy : pstrategy -> string
val pstrategy_of_string : string -> pstrategy option
val string_of_strategy : strategy -> string
val strategy_of_string : string -> strategy option
val string_of_eval : eval -> string
val eval_of_string : string -> eval option
val string_of_simulator_option : simulator option -> string
val simulator_option_of_string : string -> simulator option
