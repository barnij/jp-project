(* module Syntax: syntax trees and associated support functions *)

(* Data type definitions *)

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

type binding = VarBind

type command =
  | Eval of term
  | Bind of string * binding

(* Contexts *)
type context
val emptycontext : context 
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : context -> int -> string
val getbinding : context -> int -> binding
val name2index : context -> string -> int
val isnamebound : context -> string -> bool

(* Shifting and substitution *)

(* Printing *)


