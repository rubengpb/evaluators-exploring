type eval =
  | Normal
  | CallByValue
  | CallByName
  | ApplicativeOrder
  | HeadReduction
  | HeadSpine
  | StricNormalisation
  | HybridNormalOrder
  | HybridApplicativeOrder
  | AheadMachine
  | HeadApplicativeOrder
  | SpineApplicativeOrder
  | BalancedSpineApplicativeOrder
  | SmallStepNormalOrder
  | SmallStepApplicativeOrder
  | ReadBackCallByValue
  | ReadBackCallByName
  | ReadBackNormalOrder
  | ReadBackAheadMachine
  | ReadBackUnnamed

val string_of_eval : eval -> string

val eval_of_string : string -> eval option
