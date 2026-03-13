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

let string_of_eval = function
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
  | SmallStepNormalOrder -> "SmallStepNormalOrder"
  | SmallStepApplicativeOrder -> "SmallStepApplicativeOrder"
  | ReadBackCallByValue -> "ReadBackCallByValue"
  | ReadBackCallByName -> "ReadBackCallByName "
  | ReadBackNormalOrder -> "ReadBackNormalOrder "
  | ReadBackAheadMachine -> "ReadBackAheadMachine "
  | ReadBackUnnamed -> "ReadBackUnnamed "

let eval_of_string = function
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
  | "SmallStepNormalOrder" -> Some SmallStepNormalOrder
  | "SmallStepApplicativeOrder" -> Some SmallStepApplicativeOrder
  | "ReadBackCallByValue" -> Some ReadBackCallByValue
  | "ReadBackCallByName" -> Some ReadBackCallByName
  | "ReadBackNormalOrder" -> Some ReadBackNormalOrder
  | "ReadBackAheadMachine" -> Some ReadBackAheadMachine
  | "ReadBackUnnamed" -> Some ReadBackUnnamed
  | _ -> None
