open Syntax

val ps : string -> unit
val pc : char -> unit
val pretty_printer : term -> unit
val print_env : mvalue -> unit
val print_env_list : mvalue env -> unit
val print_stack : stack -> unit