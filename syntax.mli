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

(* Contexts *)

(* Shifting and substitution *)

(* Printing *)


