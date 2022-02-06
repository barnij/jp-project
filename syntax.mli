(* module Syntax: syntax trees and associated support functions *)

(* Data type definitions *)

type term =
  | TmVar of string
  | TmVarI of int
  | TmAbs of string * term
  | TmAbsI of term
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


type 'a env = 'a list

type mvalue =
  | Clo of term * mvalue env

type stack =
  | E_mt
  | E_arg of term * mvalue env * stack

(* Sugar syntax *)
val ctrue : term
val cfalse : term
val cand : term
val cif : term
val czero : term
val csucc : term
val cadd : term
val cmul : term
val cpred : term
val csub : term
val iszero : term
val isleq : term
val iseq : term
val fix_foo1 : term
val fix_foo2 : term
val fix : term
val cnum : int -> term
val pair : term
val pair_fst : term
val pair_snd : term
val app_helper1 : term -> term -> term
val app_helper2 : term -> term -> term -> term
val app_helper3 : term -> term -> term -> term -> term
val cnil : term
val ccons : term
val chead : term
val ctail : term
val isnil : term

(* Contexts *)

(* Shifting and substitution *)

(* Printing *)


