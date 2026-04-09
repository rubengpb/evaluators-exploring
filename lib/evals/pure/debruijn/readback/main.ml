open Eval

let evalreadback_of_short_string = function
  | "id" -> fun x -> x
  | "rn" -> failwith "TODO: Rbno.rn"
  | "bodies" -> failwith "TODO: Rbbv.bodies"
  | "bodies2" -> failwith "TODO: Rbam.bodies2"
  | "bodies3" -> failwith "TODO: Rbun.bodies3"
  | "args" -> failwith "TODO: Rbbn.args"
  | "bv" -> Evalapply.Bv.bv
  | "bn" -> Evalapply.Bn.bn
  | "he" -> Evalapply.He.he
  | s -> failwith @@ "ERROR: wrong short string: " ^ s

let eval_readback str t =
  match str with
    | Gen params -> (
          match List.map evalreadback_of_short_string params with
          | [la_1; la_2; ar2_1; ar2_2] ->
            Gen.gen (Fun.compose la_1 la_2) (Fun.compose ar2_1 ar2_2) t
        | _ -> failwith "ERROR: incorrect number of params in Pure Gen EvalApply"
      )
    | One name -> (
      match name with
        | CallByValue -> Rbbv.rbbv t
        | CallByName -> Rbbn.rbbn t
        | NormalOrder -> Rbno.rbno t
        | AheadMachine -> Rbam.rbam t
        | _ -> Rbun.rbun t
    )
