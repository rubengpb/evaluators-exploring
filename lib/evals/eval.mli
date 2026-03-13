type strategy =
  | CallByValue
  | CallByName
  | ApplicativeOrder
  | NormalOrder
  | HeadReduction
  | HeadSpine
  | StricNormalisation
  | HybridNormalOrder
  | HybridApplicativeOrder
  | AheadMachine
  | HeadApplicativeOrder
  | SpineApplicativeOrder
  | BalancedSpineApplicativeOrder

type style =
  | Apply
  | ReadBack
  | SmallStep

type one_eval = {
  strategy : strategy;
  style: style;
}

type gen_eval = {
  style :style;
  params: string list;
}

type eval =
  | One of one_eval
  | Gen of gen_eval

val string_of_eval : eval -> string
val eval_of_string : string -> eval option
val string_of_strategy : strategy -> string
val strategy_of_string : string -> strategy option
