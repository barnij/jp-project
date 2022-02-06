open Syntax

let ps = print_string
let pc = print_char

let rec pretty_printer (t: term) = match t with
  | TmVar(s)      -> ps s
  | TmVarI(n)     -> print_int n
  | TmAbs(x, t1)   -> ps ("(λ"^x^"."); pretty_printer t1; pc ')'
  | TmAbsI(t1)    -> ps "(λ."; pretty_printer t1; pc ')'
  | TmApp(t1, t2) -> pc '('; pretty_printer t1;
                     pc ' '; pretty_printer t2; pc ')'
  | TmNum(n)      -> print_int n
  | TmAdd(t1, t2) -> ps "(add "; pretty_printer t1;
                     pc ' '; pretty_printer t2; ps ")";
  | TmMul(t1, t2) -> ps "(mul "; pretty_printer t1; pc ' ';
                     pretty_printer t2; pc ')';
  | TmSub(t1, t2) ->  ps "(sub "; pretty_printer t1;
                      pc ' '; pretty_printer t2; pc ')';
  | TmEq(t1, t2) -> ps "(eq "; pretty_printer t1;
                    pc ' '; pretty_printer t2; pc ')';
  | TmTrue            -> ps "true"
  | TmFalse           -> ps "false"
  | TmIf(t1, t2, t3)  -> ps "(if ";
                        pretty_printer t1;
                        ps " then ";
                        pretty_printer t2;
                        ps " else ";
                        pretty_printer t3;
                        ps ")";
  | TmPair(t1, t2)    -> ps "(";
                         pretty_printer t1;
                         ps ",";
                         pretty_printer t2;
                         ps ")";
  | TmFix(t1)         -> ps "(fix "; pretty_printer t1; pc ')'
  | TmFst(t1)         -> ps "(fst "; pretty_printer t1; pc ')'
  | TmSnd(t1)         -> ps "(snd "; pretty_printer t1; pc ')'
  | TmNil             -> ps "nil"
  | TmCons(t1, t2)    -> ps "(cons "; pretty_printer t1;
                         pc ' '; pretty_printer t2; pc ')';
  | TmHead(t1)         -> ps "(head "; pretty_printer t1; pc ')'
  | TmTail(t1)         -> ps "(tail "; pretty_printer t1; pc ')'
  | TmIsNil(t1)         -> ps "(isnil "; pretty_printer t1; pc ')'

let rec print_env (v: mvalue) =
let Clo(t, e) = v in
  pretty_printer t; print_env_list e; ps ", "
and
print_env_list (e1: mvalue env) =
  ps "["; List.iter (fun x -> print_env x) e1; ps "]"

let print_stack (ss: stack) =
  let rec print s =
    match s with
    | [] -> ()
    | (t1,e)::s1 -> pretty_printer t1; print_env_list e; if s1=[] then () else ps " | "; print s1
  in
  pc '{'; print ss; pc '}'