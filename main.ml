open Parser
open Syntax
open Lexer
open Array
open Sys
open Printer
open Machine

let rec remove_sugar (t: term) : term = match t with
  | TmNum(n)   -> cnum n
  | TmAdd(t1,t2) -> app_helper2 cadd (remove_sugar t1) (remove_sugar t2)
  | TmMul(t1,t2) -> app_helper2 cmul (remove_sugar t1) (remove_sugar t2)
  | TmSub(t1,t2) -> app_helper2 csub (remove_sugar t1) (remove_sugar t2)
  | TmEq(t1,t2) -> app_helper2 iseq (remove_sugar t1) (remove_sugar t2)
  | TmTrue -> ctrue
  | TmFalse -> cfalse
  | TmIf(t1, t2, t3) -> app_helper3 cif
                        (remove_sugar t1) (remove_sugar t2)(remove_sugar t3)
  | TmFix(t1) -> app_helper1 fix (remove_sugar t1)
  | TmPair(t1, t2) -> app_helper2 pair (remove_sugar t1) (remove_sugar t2)
  | TmFst(t1) -> app_helper1 pair_fst (remove_sugar t1)
  | TmSnd(t1) -> app_helper1 pair_snd (remove_sugar t1)
  | TmNil -> cnil
  | TmCons(t1, t2) -> app_helper2 ccons (remove_sugar t1) (remove_sugar t2)
  | TmHead(t1) -> app_helper1 chead (remove_sugar t1)
  | TmTail(t1) -> app_helper1 ctail (remove_sugar t1)
  | TmIsNil(t1) -> app_helper1 isnil (remove_sugar t1)
  | _ -> t

let rec remove_names (tt: term) : term =
  let rec get_ind l x i = match l with
  | h::t when h = x -> i
  | h::t -> get_ind t x (i+1)
  | [] -> -1
  in let rec foo (t: term) (l: string list) =
  match t with
  | TmVar(x)        -> let ind = get_ind l x 0 in 
                        if ind = -1 then TmVar("g"^get_iden()) else TmVarI(ind)
  | TmApp(t1, t2)   -> TmApp(foo t1 l, foo t2 l)
  | TmAbs(x, t1)    -> TmAbsI(foo t1 (x::l))
  | _ as other      -> other
  in foo tt []


let main () =
  let cin =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel cin in
    let raw_res = try Parser.toplevel Lexer.main lexbuf
    with Parsing.Parse_error -> print_string "Parse error"; failwith "Parser error"
    in pretty_printer raw_res; pc '\n';
    let desugar_res = remove_sugar raw_res in
    pretty_printer desugar_res; pc '\n';
    let res = remove_names desugar_res in
    pretty_printer res; pc '\n';
    let (wt1, _, _) = eval_normal res in pretty_printer wt1; pc '\n'

let _ = main ()