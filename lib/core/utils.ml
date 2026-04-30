open Lexer
open Parser
open Syntax

let rec string_of_cterm = function
  | CVar x -> x
  | CAbs (x, t) -> "\\" ^ x ^ "." ^ string_of_cterm t
  | CApp (CVar x1, CVar x2) -> x1 ^ " " ^ x2
  | CApp (CVar x, t) -> x ^ " (" ^ string_of_cterm t ^ ")"
  | CApp (CApp(t1, t2), CVar x) -> string_of_cterm (CApp (t1, t2)) ^ " " ^ x
  | CApp (CApp(t1, t2), CAbs(x,t)) -> string_of_cterm (CApp (t1, t2)) ^ " (" ^ string_of_cterm (CAbs(x,t)) ^ ")"
  | CApp (CApp(t1, t2), t) -> string_of_cterm (CApp (t1, t2)) ^ " (" ^ string_of_cterm t ^ ")"
  | CApp (t, CVar x) -> "(" ^ string_of_cterm t ^ ") " ^ x
  | CApp (t1, t2) -> "(" ^ string_of_cterm t1 ^ ") (" ^ string_of_cterm t2 ^ ")"
  | Clou (t, ctxt) -> "<" ^ string_of_cterm t ^ ", " ^ string_of_ctxt ctxt ^ ">"
and string_of_ctxt ctxt =
  let subs = List.map (fun (v, ct) -> v ^ " -> " ^ string_of_cterm ct) ctxt in
  "[" ^ String.concat ", " subs ^ "]"

let rec string_of_pterm = function
  | Var x -> x
  | Abs (x, t) ->
      "\\" ^ x ^ "." ^ string_of_pterm t
  | App (Var x1, Var x2) -> x1 ^ " " ^ x2
  | App (Var x, t) -> x ^ " (" ^ string_of_pterm t ^ ")"
  | App (App(t1, t2), Var x) -> string_of_pterm (App (t1, t2)) ^ " " ^ x
  | App (App(t1, t2), Abs(x,t)) -> string_of_pterm (App (t1, t2)) ^ " (" ^ string_of_pterm (Abs(x,t)) ^ ")"
  | App (App(t1, t2), t) -> string_of_pterm (App (t1, t2)) ^ " (" ^ string_of_pterm t ^ ")"
  | App (t, Var x) -> "(" ^ string_of_pterm t ^ ") " ^ x
  | App (t1, t2) -> "(" ^ string_of_pterm t1 ^ ") (" ^ string_of_pterm t2 ^ ")"

let index_of x ctx =
  let rec aux i = function
    | [] -> None
    | y :: ys -> if x = y then Some i else aux (i + 1) ys
  in
  aux 0 ctx

let rec dbterm_of_pterm_aux ctx = function
  | Var x -> (
    match index_of x ctx with
      | None -> FDBVar x
      | Some n -> DBVar n
    )
  | Abs (x, t) ->
      DBAbs (dbterm_of_pterm_aux (x :: ctx) t)
  | App (m, n) ->
      DBApp (dbterm_of_pterm_aux ctx m, dbterm_of_pterm_aux ctx n)

let dbterm_of_pterm t = dbterm_of_pterm_aux [] t

let rec pterm_of_dbterm t =
  let rec aux ctx counter = function
    | FDBVar x -> (Var x, counter)
    | DBVar k ->
        (try (Var (List.nth ctx k), counter)
         with _ -> failwith "Bad number inside De Bruijn term")
    | DBAbs b ->
        let x = "x" ^ string_of_int counter in
        let (body, counter') = aux (x :: ctx) (counter + 1) b in
        (Abs (x, body), counter')
    | DBApp (m, n) ->
        let (m', c1) = aux ctx counter m in
        let (n', c2) = aux ctx c1 n in
        (App (m', n'), c2)
  in
  fst (aux [] 0 t)

let string_of_dbterm_pure dbt = string_of_pterm @@ pterm_of_dbterm dbt

let rec string_of_dbterm = function
  | FDBVar x -> x
  | DBVar i ->
      string_of_int i
  | DBAbs b ->
      "\\." ^ string_of_dbterm b
  | DBApp (DBVar x1, DBVar x2) -> string_of_int x1 ^ " " ^ string_of_int x2
  | DBApp (FDBVar x1, DBVar x2) -> x1 ^ " " ^ string_of_int x2
  | DBApp (DBVar x1, FDBVar x2) -> string_of_int x1 ^ " " ^ x2
  | DBApp (FDBVar x1, FDBVar x2) -> x1 ^ " " ^ x2
  | DBApp (DBVar x, t) -> string_of_int x ^ " (" ^ string_of_dbterm t ^ ")"
  | DBApp (FDBVar x, t) -> x ^ " (" ^ string_of_dbterm t ^ ")"
  | DBApp (DBApp(t1, t2), DBVar x) -> string_of_dbterm (DBApp (t1, t2)) ^ " " ^ string_of_int x
  | DBApp (DBApp(t1, t2), FDBVar x) -> string_of_dbterm (DBApp (t1, t2)) ^ " " ^ x
  | DBApp (DBApp(t1, t2), DBAbs t) -> string_of_dbterm (DBApp (t1, t2)) ^ " (" ^ string_of_dbterm (DBAbs t) ^ ")"
  | DBApp (DBApp(t1, t2), t) -> string_of_dbterm (DBApp (t1, t2)) ^ " (" ^ string_of_dbterm t ^ ")"
  | DBApp (t, DBVar x) -> "(" ^ string_of_dbterm t ^ ") " ^ string_of_int x
  | DBApp (t, FDBVar x) -> "(" ^ string_of_dbterm t ^ ") " ^ x
  | DBApp (t1, t2) -> "(" ^ string_of_dbterm t1 ^ ") (" ^ string_of_dbterm t2 ^ ")"

let string_of_term = function
  | TPure t -> string_of_pterm t
  | TClousure t -> string_of_cterm t
  | TDeBruijn t -> string_of_dbterm_pure t

let rec clousure_of_pure = function
    | Var x -> CVar x
    | Abs (x, b) -> CAbs (x, clousure_of_pure b)
    | App (t1, t2) -> CApp (clousure_of_pure t1, clousure_of_pure t2)


let rec free_vars = function
  | Var x -> [x]
  | Abs (x, t) ->
      List.filter (fun y -> y <> x) (free_vars t)
  | App (t1, t2) ->
    List.sort_uniq String.compare @@ free_vars t1 @ free_vars t2

let rec new_free_var x xs =
  let y = x ^ "s" in
  if List.mem y xs then new_free_var y xs
  else y

let rec subst n x b =
  match b with
    | Var y ->
        if y = x then n else b
    | App (t1, t2) ->
        App (subst n x t1, subst n x t2)
    | Abs (y, body) ->
      if x = y then b
      else (
          let free_body = free_vars body in
          let free_n = free_vars n in
          if not (List.mem x free_body) then b
          else if not (List.mem y free_n) then Abs (y, subst n x body)
          else let z = new_free_var y free_body in
            Abs (z, subst n x (subst (Var z) y body))
        )

let rec shift n x l =
  match n with
    | FDBVar x -> n
    | DBVar k -> if k >= l then DBVar (k + x) else DBVar k
    | DBApp (t1, t2) -> DBApp(shift t1 x l, shift t2 x l)
    | DBAbs body -> DBAbs (shift body x (l+1))

let rec subst_db n x b =
  match b with
    | FDBVar x -> b
    | DBVar k ->
        if k = x then (shift n x 0)
        else if k < x then b
        else DBVar (k - 1)
    | DBApp (t1, t2) -> DBApp(subst_db n x t1, subst_db n x t2)
    | DBAbs body -> DBAbs (subst_db n (x + 1) body)

let rec alpha_equiv t1 t2 =
  match (t1, t2) with
    | (Var x, Var y) -> x = y
    | (Abs(x, b1), Abs(y, b2)) ->
      if x = y then alpha_equiv b1 b2
      else alpha_equiv b1 @@ subst (Var x) y b2
    | (App(t1, t2), App(tt1, tt2)) -> alpha_equiv t1 tt1 && alpha_equiv t2 tt2
    | _ -> false

let rec is_there_redex = function
  | Var _ -> false
  | Abs(_, body) -> is_there_redex body
  | App(Abs(_, _), _) -> true
  | App(t1, t2) -> is_there_redex t1 || is_there_redex t2

let rec term_of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.main read lexbuf

let rec plug t = function
  | Top -> t
  | AppL (ctx, n) -> plug (App (t, n)) ctx
  | AppR (m, ctx) -> plug (App (m, t)) ctx
  | AbsC (x, ctx) -> plug (Abs (x, t)) ctx
