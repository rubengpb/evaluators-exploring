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

let string_to_strategy = function
  | "Normal" -> Some Normal
  | "CallByValue" -> Some CallByValue
  | "CallByName" -> Some CallByName
  | "ApplicativeOrder" -> Some ApplicativeOrder
  | "HeadReduction" -> Some HeadReduction
  | "HeadSpine" -> Some HeadSpine
  | "StricNormalisation" -> Some StricNormalisation
  | "HybridNormalOrder" -> Some HybridNormalOrder
  | "HybridApplicativeOrder" -> Some HybridApplicativeOrder
  | "AheadMachine" -> Some AheadMachine
  | "HeadApplicativeOrder" -> Some HeadApplicativeOrder
  | "SpineApplicativeOrder" -> Some SpineApplicativeOrder
  | "BalancedSpineApplicativeOrder" -> Some BalancedSpineApplicativeOrder
  | _ -> None
