open Syntax
open Printer

let printer t e c txt = ps txt; pretty_printer t; pc ' '; print_env_list e; pc ' '; print_stack c; ps "\n"

(* Maszyna Krivine'a ze SKOSA, autorstwa M. Orłowskiej *)
let rec eval (t: term)  (e: mvalue env) (c: stack) =
  (* pretty_printer t; pc ' ';
  print_env_list e; pc ' ';
  print_stack c; ps "\n"; debugi *)
  match t with
  | TmVarI n -> 
    let Clo(t', e') = List.nth e n in eval t' e' c
  | TmVar id -> t, e, c
  | TmAbsI s ->
    ( match c with
      | [] -> t, e, c
      | (t', e')::c' ->
        eval s (Clo(t', e')::e) c' )
  | TmApp (t1, t2) ->
    eval t1 e ((t2, e)::c)
  | _ -> failwith "Eval error"

let rec eval_Krivine (t : term) =
  eval t [] []

(*****************************************************)

let uniq_iden = ref 0
let set_iden n = uniq_iden := n
let get_iden x = incr uniq_iden; (fun _ -> string_of_int !uniq_iden) x

let rec makeFreeVars iden t level =
  match t with
  | TmVarI i when i = level -> TmVar(iden)
  | TmAbsI t1       -> TmAbsI (makeFreeVars iden t1 (level+1))
  | TmApp(t1, t2)   -> TmApp(makeFreeVars iden t1 level, makeFreeVars iden t2 level)
  | _               -> t

let rec backVars iden t level =
  match t with
  | TmVar s when s = iden -> TmVarI(level)
  | TmApp(t1, t2)   -> TmApp(backVars iden t1 level, backVars iden t2 level)
  | TmAbsI t1       -> TmAbsI(backVars iden t1 (level + 1))
  | _               -> t

let getDummyEnv (e: mvalue env) = (Clo(TmVar("DUMMY"), []))::e
let getUndummyEnv (e: mvalue env) = match e with
  | Clo(TmVar("DUMMY"), _)::t -> t
  | _ -> e

let rec is_good_a t =
  match t with
  | TmVar _ -> true
  | TmApp(t1, t2) -> is_good_a t1 && is_good_n t2
  | _  -> false
and is_good_n t =
  match t with
  | TmAbsI n -> is_good_n n
  | _ -> is_good_a t

let is_beta_normal t = is_good_n t

let rec make_term_from_env (t: term) (e: mvalue env) =
  match t with
  | TmVarI n  -> (
    match List.nth_opt e n with
    | Some(Clo(t', e')) -> t'
    | None -> t
  )
  | TmAbsI t' -> TmAbsI (make_term_from_env t' e)
  | TmApp(t1, t2) -> TmApp(make_term_from_env t1 e, make_term_from_env t2 e)
  | _ -> t

let rec eval_normal_foo (t: term) (e: mvalue env) =
  let go_under_lambda t e =
    let iden = "f"^get_iden() in
    let t' = makeFreeVars iden t 0 in
    let go_t = eval_normal_foo t' (getDummyEnv e) in
    let go_t' = backVars iden go_t 0 in
    TmAbsI(go_t')
  in
  let go_with_cont (tt: term) (c: stack) =
    let cont = List.map (fun (t, e) -> eval_normal_foo t e) c
    in List.fold_left (fun acc el -> TmApp(acc, el)) tt cont
  in
  if is_beta_normal t
  then make_term_from_env t e else
  let t1, e1, c1 = eval t e [] in
  let nt = (
    match t1 with
    | TmAbsI s -> go_under_lambda s e1
    | TmVar _ | TmVarI _   -> t1
    | _ -> failwith "evaluation error: bad contructor"
    )
  in go_with_cont nt c1

let eval_normal (t: term) =
  eval_normal_foo t []


let rec compare_terms t1 t2 =
  match t1, t2 with
  | TmApp(t1', t1''), TmApp(t2', t2'') -> compare_terms t1' t2' && compare_terms t1'' t2''
  | TmAbsI(t1'), TmAbsI(t2') -> compare_terms t1' t2'
  | TmVar _, TmVar _ -> true
  | TmVarI(n1), TmVarI(n2) when n1=n2 -> true
  | _ -> false



