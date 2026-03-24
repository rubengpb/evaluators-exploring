type language =
  | Pure
  | Clousure

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
  | EvalApply
  | ReadBack
  | SmallStep

type one_eval = {
  language : language;
  strategy : strategy;
  style: style;
}

type gen_eval = {
  language : language;
  style :style;
  params: string list;
}

type eval =
  | One of one_eval
  | Gen of gen_eval

val get_language : eval -> language
val string_of_eval : eval -> string
val eval_of_string : string -> eval option
val string_of_strategy : strategy -> string
val strategy_of_string : string -> strategy option
