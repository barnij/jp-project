open Syntax

val eval : term -> mvalue env -> stack -> term * mvalue env * stack
val eval_Krivine : term -> term * mvalue env * stack
val uniq_iden : int ref
val set_iden : int -> unit
val get_iden : unit -> string

val makeFreeVars : string -> term -> int -> term
val backVars : string -> term -> int -> term
val getDummyEnv : mvalue env -> mvalue env
val getUndummyEnv : mvalue env -> mvalue env
val eval_normal : term -> term * mvalue env * stack
val printer : term -> mvalue env -> stack -> string -> unit