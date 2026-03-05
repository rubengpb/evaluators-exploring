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

let strategy_to_string = function
  | Normal -> "Normal"
  | CallByValue -> "CallByValue"
  | CallByName -> "CallByName"
  | ApplicativeOrder -> "ApplicativeOrder"
  | HeadReduction -> "HeadReduction"
  | HeadSpine -> "HeadSpine"
  | StricNormalisation -> "StricNormalisation"
  | HybridNormalOrder -> "HybridNormalOrder"
  | HybridApplicativeOrder -> "HybridApplicativeOrder"
  | AheadMachine -> "AheadMachine"
  | HeadApplicativeOrder -> "HeadApplicativeOrder"
  | SpineApplicativeOrder -> "SpineApplicativeOrder"
  | BalancedSpineApplicativeOrder -> "BalancedSpineApplicativeOrder"
