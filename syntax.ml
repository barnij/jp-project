open Format

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type term =
  | TmVar of string
  | TmAbs of string * term
  | TmApp of term * term

  | TmNum of int
  | TmAdd of term * term
  | TmMul of term * term
  | TmSub of term * term
  | TmEq of term * term

  | TmTrue
  | TmFalse
  | TmIf of term * term * term

  | TmFix of term

  | TmPair of term * term
  | TmFst of term
  | TmSnd of term

  | TmNil
  | TmCons of term * term
  | TmHead of term
  | TmTail of term
  | TmIsNil of term

type command =
  | Eval of term
  | Bind of string

(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x = x::ctx

let addname ctx x = addbinding ctx x

let rec isnamebound ctx x =
  match ctx with
    | [] -> false
    | y::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else (x::ctx), x

let index2name fi ctx x =
  try
    let xn = List.nth ctx x in
    xn with 
    | Failure _ -> let msg = Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
      failwith (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
      [] -> failwith ("Identifier " ^ x ^ " is unbound")
    | y::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)


(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0


