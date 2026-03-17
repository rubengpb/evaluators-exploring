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
  | Apply
  | ReadBack
  | SmallStep

let apply_params = ["id"; "bv"; "bn"; "ao"; "no"; "hr"; "he"; "sn"; "hn"; "ha"; "am"; "ho"; "so"; "bs"]

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

let string_of_language = function
  | Pure -> "Pure"
  | Clousure -> "Clousure"

let language_of_string = function
  | "Pure" -> Some Pure
  | "Clousure" -> Some Clousure
  | _ -> None

let string_of_strategy = function
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

let strategy_of_string = function
  | "CallByValue" -> Some CallByValue
  | "CallByName" -> Some CallByName
  | "ApplicativeOrder" -> Some ApplicativeOrder
  | "NormalOrder" -> Some NormalOrder
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

let string_of_style = function
  | Apply -> "Apply"
  | ReadBack -> "ReadBack"
  | SmallStep -> "SmallStep"

let string_of_eval = function
  | One eval ->
    String.concat "_"
      [
        string_of_language eval.language;
        string_of_style eval.style;
        string_of_strategy eval.strategy
      ]
  | Gen eval ->
    String.concat "_"
      ([
        string_of_language eval.language;
        "Gen";
        string_of_style eval.style;
      ] @ eval.params)

let eval_of_string s =
  let lst = String.split_on_char '_' s in
  match lst with
    | "Gen" :: "Apply" :: params ->
      if List.length params = 5 &&
        List.for_all (fun x -> List.mem x apply_params) params
      then
        Some (Gen {language = Pure; style = Apply; params = params })
      else None
    | "Gen" :: "ReadBack" :: params -> print_endline "TODO"; None
    | "Gen" :: "SmallStep" :: params -> print_endline "TODO"; None
    | ["Clousure"; "Apply"; str] -> (
     match strategy_of_string str with
      | Some s -> Some (One { language = Clousure; strategy = s; style = Apply })
      | None -> None)
    | ["Apply"; str] -> (
     match strategy_of_string str with
      | Some s -> Some (One { language = Pure; strategy = s; style = Apply })
      | None -> None)
    | ["ReadBack"; str] -> (
     match strategy_of_string str with
      | Some s -> Some (One { language = Pure; strategy = s; style = ReadBack })
      | None -> None)
    | ["SmallStep"; str] -> (
     match strategy_of_string str with
      | Some s -> Some (One { language = Pure; strategy = s; style = SmallStep })
      | None -> None)
    | _ -> None
