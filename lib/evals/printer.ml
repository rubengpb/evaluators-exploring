open Core.Syntax
open Core.Utils

let color_code color =
  match String.lowercase_ascii color with
  | "black" -> "30"
  | "red" -> "31"
  | "green" -> "32"
  | "yellow" -> "33"
  | "blue" -> "34"
  | "magenta" -> "35"
  | "cyan" -> "36"
  | "white" -> "37"
  | _ -> "37"

let colorize text color =
  let code = color_code color in
  Printf.sprintf "\027[1;4;%sm%s\027[0m" code text

let string_of_zipper t z_ctxt = string_of_pterm @@ plug t z_ctxt

let string_of_redex = function
    | App(Abs(x,b), Var y) ->
      colorize ("(\\" ^ x ^ "." ^ string_of_pterm b ^ ")") "red" ^ " " ^
      colorize y "blue"
    | App(Abs(x,b), n) ->
      colorize ("(\\" ^ x ^ "." ^ string_of_pterm b ^ ")") "red" ^ " " ^
      colorize ("(" ^ string_of_pterm n ^ ")") "blue"
    | _ -> failwith "Error: Not redex to print"

let string_of_db_redex = function
    | DBApp(DBAbs b, FDBVar y) ->
      colorize ("(\\" ^ "." ^ string_of_dbterm b ^ ")") "red" ^ " " ^
      colorize y "blue"
    | DBApp(DBAbs b, DBVar y) ->
      colorize ("(\\" ^ "." ^ string_of_dbterm b ^ ")") "red" ^ " " ^
      colorize (string_of_int y) "blue"
    | DBApp(DBAbs b, n) ->
      colorize ("(\\" ^ "." ^ string_of_dbterm b ^ ")") "red" ^ " " ^
      colorize ("(" ^ string_of_dbterm n ^ ")") "blue"
    | _ -> failwith "Error: Not redex to print"

let string_of_c_redex = function
    | CApp(CAbs(x,b), CVar y) ->
      colorize ("(\\" ^ x ^ "." ^ string_of_cterm b ^ ")") "red" ^ " " ^
      colorize y "blue"
    | CApp(CAbs(x,b), n) ->
      colorize ("(\\" ^ x ^ "." ^ string_of_cterm b ^ ")") "red" ^ " " ^
      colorize ("(" ^ string_of_cterm n ^ ")") "blue"
    | CApp(Clou(CAbs(x,b), env) as cl, n) ->
      colorize ("(" ^ string_of_cterm cl ^ ")") "red" ^ " " ^
      colorize ("(" ^ string_of_cterm n ^ ")") "blue"
    | Clou(CApp(CAbs(x,b) as abs , n), env)->
      "<" ^
      colorize ("(" ^ string_of_cterm abs ^ ")") "red" ^ " " ^
      colorize ("(" ^ string_of_cterm n ^ ")") "blue" ^
      ", " ^ string_of_ctxt env ^ ">"
    | Clou(CApp(Clou(CAbs(x,b), env) as abs , n), env0)->
      "<" ^
      colorize ("(" ^ string_of_cterm abs ^ ")") "red" ^ " " ^
      colorize ("(" ^ string_of_cterm n ^ ")") "blue" ^
      ", " ^ string_of_ctxt env0 ^ ">"
    | wt -> failwith ("Error: Not redex to print: " ^ string_of_cterm wt)

let rec plug_str t_str z_ctxt =
  match z_ctxt with
    | Top -> t_str
    | AppL (ctx, Var x) ->
        plug_str (t_str ^ " " ^ x) ctx
    | AppL (ctx, n) ->
        plug_str (t_str ^ " (" ^ string_of_pterm n ^ ")") ctx
    | AppR (Var x, ctx) ->
        plug_str (x ^ " (" ^ t_str ^ ")") ctx
    | AppR (Abs(x,m) as abs, ctx) ->
        plug_str ("(" ^ string_of_pterm abs ^ ") (" ^ t_str ^ ")") ctx
    | AppR (m, ctx) ->
        plug_str (string_of_pterm m ^ " (" ^ t_str ^ ")") ctx
    | AbsC (x, Top) -> "\\" ^ x ^ "." ^ t_str
    | AbsC (x, AbsC (y, ctx)) ->
      plug_str ("\\" ^ x ^ "." ^ t_str) (AbsC(y, ctx))
    | AbsC (x, ctx) ->
      plug_str ("(\\" ^ x ^ "." ^ t_str ^ ")") ctx

let rec plug_db_str t_str z_ctxt =
  match z_ctxt with
    | DBTop -> t_str
    | DBAppL (ctx, FDBVar x) ->
        plug_db_str (t_str ^ " " ^ x) ctx
    | DBAppL (ctx, DBVar x) ->
        plug_db_str (t_str ^ " " ^ string_of_int x) ctx
    | DBAppL (ctx, n) ->
        plug_db_str (t_str ^ " (" ^ string_of_dbterm n ^ ")") ctx
    | DBAppR (m, ctx) ->
        plug_db_str (string_of_dbterm m ^ " (" ^ t_str ^ ")") ctx
    | DBAbsC DBTop -> "\\." ^ t_str
    | DBAbsC (DBAbsC ctx) ->
      plug_db_str ("\\." ^ t_str) (DBAbsC ctx)
    | DBAbsC ctx ->
      plug_db_str ("(\\." ^ t_str ^ ")") ctx

let rec plug_c_str t_str z_ctxt =
  match z_ctxt with
    | CTop -> t_str
    | CAppL (ctx, CVar x) ->
        plug_c_str (t_str ^ " " ^ x) ctx
    | CAppL (ctx, n) ->
        plug_c_str (t_str ^ " (" ^ string_of_cterm n ^ ")") ctx
    | CAppR (m, ctx) ->
        plug_c_str (string_of_cterm m ^ " (" ^ t_str ^ ")") ctx
    | CAbsC (x,CTop) -> "\\" ^ x ^ "." ^ t_str
    | CAbsC (x, CAbsC(y, ctx)) ->
      plug_c_str ("\\" ^ x ^ "." ^ t_str) (CAbsC(y, ctx))
    | CAbsC (x, ctx) ->
      plug_c_str ("(\\" ^ x ^ "." ^ t_str ^ ")") ctx
    | CClouC (z_ctxt, ctxt) ->
      plug_c_str ("<" ^ t_str ^ ", [" ^ string_of_ctxt ctxt ^ "]>") z_ctxt
