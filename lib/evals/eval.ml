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
  | StricNormalisation
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

let apply_params = ["id"; "bv"; "bn"; "ao"; "no"; "hr"; "he"; "sn"; "hn"; "ha"; "am"; "ho"; "so"; "bs"]
let readback_params = ["id";"rn";"bodies";"bodies2";"bodies3";"args";"bv";"bn";"he";]

let string_of_language = function
  | Pure -> "Pure"
  | Value -> "Value"

let language_of_string = function
  | "Pure" -> Some Pure
  | "Value" -> Some Value
  | _ -> None

let string_of_subst = function
  | Subst -> "Subst"
  | DeBruijn -> "DeBruijn"
  | Clousure -> "Clousure"

let subst_of_string = function
  | "Subst" -> Some Subst
  | "DeBruijn" -> Some DeBruijn
  | "Clousure" -> Some Clousure
  | _ -> None

let short_string_of_pstrategy = function
  | CallByValue -> "bv"
  | CallByName -> "bn"
  | ApplicativeOrder -> "ao"
  | NormalOrder -> "no"
  | HeadReduction -> "hr"
  | HeadSpine -> "he"
  | StricNormalisation -> "sn"
  | HybridNormalOrder -> "hn"
  | HybridApplicativeOrder -> "ha"
  | AheadMachine -> "ao"
  | HeadApplicativeOrder -> "ho"
  | SpineApplicativeOrder -> "so"
  | BalancedSpineApplicativeOrder -> "bs"

let string_of_pstrategy = function
  | CallByValue -> "CallByValue"
  | CallByName -> "CallByName"
  | ApplicativeOrder -> "ApplicativeOrder"
  | NormalOrder -> "NormalOrder"
  | HeadReduction -> "HeadReduction"
  | HeadSpine -> "HeadSpine"
  | StricNormalisation -> "StricNormalisation"
  | HybridNormalOrder -> "HybridNormalOrder"
  | HybridApplicativeOrder -> "HybridApplicativeOrder"
  | AheadMachine -> "AheadMachine"
  | HeadApplicativeOrder -> "HeadApplicativeOrder"
  | SpineApplicativeOrder -> "SpineApplicativeOrder"
  | BalancedSpineApplicativeOrder -> "BalancedSpineApplicativeOrder"

let pstrategy_of_string = function
  | "CallByValue" | "bv" -> Some CallByValue
  | "CallByName" | "bn"-> Some CallByName
  | "ApplicativeOrder" | "ao" -> Some ApplicativeOrder
  | "NormalOrder" | "no" -> Some NormalOrder
  | "HeadReduction" | "hr" -> Some HeadReduction
  | "HeadSpine" | "he" -> Some HeadSpine
  | "StricNormalisation" | "sn" -> Some StricNormalisation
  | "HybridNormalOrder" | "hn" -> Some HybridNormalOrder
  | "HybridApplicativeOrder" | "ha" -> Some HybridApplicativeOrder
  | "AheadMachine" | "am" -> Some AheadMachine
  | "HeadApplicativeOrder" | "ho" -> Some HeadApplicativeOrder
  | "SpineApplicativeOrder" | "so" -> Some SpineApplicativeOrder
  | "BalancedSpineApplicativeOrder" | "bs" -> Some BalancedSpineApplicativeOrder
  | _ -> None

let string_of_strategy = function
  | Gen params ->
    "Gen_" ^ (String.concat "_" params)
  | One name -> string_of_pstrategy name

let strategy_of_string str =
  let splited_str = String.split_on_char '_' str in
  match splited_str with
    | "Gen"::params ->
      if List.for_all (fun x -> List.mem x apply_params) params
        || List.for_all (fun x -> List.mem x readback_params) params
      then Some (Gen params)
      else None
    | [str] -> Option.map (fun s -> One s) (pstrategy_of_string str)
    | _ -> None

let string_of_style = function
  | EvalApply -> "EvalApply"
  | EvalApplyZipper -> "EvalApplyZipper"
  | ReadBack -> "ReadBack"
  | SmallStep -> "SmallStep"

let style_of_string = function
  | "EvalApply" | "ea" -> Some EvalApply
  | "EvalApplyZipper" | "eaz" -> Some EvalApplyZipper
  | "ReadBack" | "rb" -> Some ReadBack
  | "SmallStep" | "ss" -> Some SmallStep
  | _ -> None

let string_of_eval eval =
  String.concat "_"
  [
    string_of_language eval.language;
    string_of_subst eval.subst;
    string_of_style eval.style;
    string_of_strategy eval.strategy;
  ]

let ( let* ) = Option.bind
let eval_of_string s =
  let lst = String.split_on_char '_' s in
  match lst with
    | lang::subst::style::str ->
    let* lang = language_of_string lang in
    let* subst = subst_of_string subst in
    let* style = style_of_string style in
    let* str = strategy_of_string @@ String.concat "_" str in
    Some {language = lang; subst = subst; style = style; strategy = str}
    | _ -> None

let simulator_option_of_string str =
  let splited_str = String.split_on_char '_' str in
  match splited_str with
    | [guest_str;host_str] ->
      let* guest = pstrategy_of_string guest_str in
      let* host = pstrategy_of_string host_str in
      Some {guest = guest; host = host}
    | _ -> None

let string_of_simulator_option = function
  | None -> "None"
  | Some sim ->
    string_of_pstrategy sim.guest ^ " in " ^ string_of_pstrategy sim.host
