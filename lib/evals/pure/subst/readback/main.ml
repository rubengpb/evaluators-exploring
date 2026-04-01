open Eval

let eval_readback str t =
  match str with
    | Gen params ->
      failwith "TODO"
    | One name -> (
      match name with
        | CallByValue -> Rbbv.rbbv t
        | CallByName -> Rbbn.rbbn t
        | NormalOrder -> Rbno.rbno t
        | AheadMachine -> Rbam.rbam t
        | _ -> Rbun.rbun t
    )
