open Parser
open Syntax
open Lexer
open Array
open Sys
open Printer
open Machine

let rec remove_sugar (t: term) : term = match t with
  | TmNum(n)      -> cnum n
  | TmApp(t1, t2) -> TmApp(remove_sugar t1, remove_sugar t2)
  | TmAbs(x, t1)     -> TmAbs(x, remove_sugar t1)
  | TmAdd(t1,t2) -> app_helper2 cadd (remove_sugar t1) (remove_sugar t2)
  | TmMul(t1,t2) -> app_helper2 cmul (remove_sugar t1) (remove_sugar t2)
  | TmSub(t1,t2) -> app_helper2 csub (remove_sugar t1) (remove_sugar t2)
  | TmEq(t1,t2) -> app_helper2 iseq (remove_sugar t1) (remove_sugar t2)
  | TmTrue -> ctrue
  | TmFalse -> cfalse
  | TmIf(t1, t2, t3) -> app_helper3 cif
                        (remove_sugar t1) (remove_sugar t2) (remove_sugar t3)
  | TmFix(t1) -> app_helper1 fix (remove_sugar t1)
  | TmPair(t1, t2) -> app_helper2 pair (remove_sugar t1) (remove_sugar t2)
  | TmFst(t1) -> app_helper1 (remove_sugar t1) pair_fst
  | TmSnd(t1) -> app_helper1 (remove_sugar t1) pair_snd
  | TmNil -> cnil
  | TmCons(t1, t2) -> app_helper2 ccons (remove_sugar t1) (remove_sugar t2)
  | TmHead(t1) -> app_helper1 (remove_sugar t1) chead
  | TmTail(t1) -> app_helper1 (remove_sugar t1) ctail
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
  let cin2m =
    if Array.length Sys.argv > 2
    then Some(open_in Sys.argv.(2))
    else None
  in
  let cout = open_out "result.txt" in
  let lexbuf = Lexing.from_channel cin in
    let raw_res = try Parser.toplevel Lexer.main lexbuf
    with Parsing.Parse_error -> print_string "Parse error"; failwith "Parser error" in
    let desugar_res = remove_sugar raw_res in
    let res = remove_names desugar_res in
    let wt1 = eval_normal res in
    match cin2m with
    | None ->
      ps "raw: "; pretty_printer raw_res; pc '\n';
      ps "wihout sugar: "; pretty_printer desugar_res; pc '\n';
      ps "new indexes: ";pretty_printer res; pc '\n';
      ps "result: "; pretty_printer wt1; pc '\n';
      output_string cout ((pretty_printer_string wt1)^"\n")
    | Some(cin2) ->
      let lexbuf2 = Lexing.from_channel cin2 in
      let raw_res2 = try Parser.toplevel Lexer.main lexbuf2
        with Parsing.Parse_error -> print_string "Parse error2"; failwith "Parser error2"
      in let wt2 = eval_normal (remove_names (remove_sugar raw_res2)) in
      ps "term 1:"; pretty_printer raw_res; pc '\n';
      ps "result 1: "; pretty_printer wt1 ; pc '\n';
      ps "term 2:"; pretty_printer raw_res2; pc '\n';
      ps "result 2: "; pretty_printer wt2 ; pc '\n';
      if compare_terms wt1 wt2 then
        (ps "terms are Beta-equal\n"; output_string cout "true\n")
      else (ps "terms are different\n"; output_string cout "false\n")

let _ = main ()