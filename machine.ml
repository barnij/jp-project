open Syntax
open Printer

let printer t e c txt = ps txt; pretty_printer t; pc ' '; print_env_list e; pc ' '; print_stack c; ps "\n"

let rec eval (t: term)  (e: mvalue env) (c: stack) =
  pretty_printer t; pc ' ';
  print_env_list e; pc ' ';
  print_stack c; ps "\n";
  match t with
  | TmVarI n -> 
    let Clo(t', e') = List.nth e n in eval t' e' c
  | TmVar id -> t, e, c
  | TmAbsI s ->
    ( match c with
      | E_mt -> t, e, c
      | E_arg (t', e', c') ->
        eval s (Clo(t', e')::e) c' )
  | TmApp (t1, t2) ->
    eval t1 e (E_arg (t2, e, c))
  | _ -> failwith "Eval error"

let rec eval_Krivine (t : term) =
  eval t [] E_mt

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

let rec eval_normal_foo (t: term) (e: mvalue env) (c: stack) =
  let t1, e1, c1 = eval t e c in
  match t1 with
  | TmAbsI t2 -> (
    let iden = "f"^get_iden() in
    let t2' = makeFreeVars iden t2 0 in
    printer t2' (getDummyEnv e1) c1 "pr2:";
    let (t3, e2, c2) = eval_normal_foo t2' (getDummyEnv e1) c1 in
    printer t3 e2 c2 "pr4:";
    let t4 = TmAbsI(backVars iden t3 0) in
    printer t4 (getUndummyEnv e2) c2 "pr3:";
    t4, (getUndummyEnv e2), c2 )
  | _ -> printer t1 e1 c1 "pr7:"; t1, e1, c1

let eval_normal (t: term) =
  eval_normal_foo t [] E_mt