type strategy =
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

val strategy_to_string : strategy -> string

val string_to_strategy : string -> strategy option
