open Parser
open Syntax
open Lexer
open Array
open Sys

let rec pretty_printer (t: term) = match t with
  | TmVar(s)      -> print_string s
  | TmAbs(x, t)   -> print_string ("(λ"^x^".");
                     pretty_printer t;
                     print_string ")"
  | TmApp(t1, t2) -> print_char '(';
                     pretty_printer t1;
                     print_char ' ';
                     pretty_printer t2;
                     print_char ')'
  | TmNum(n)      -> print_int n
  | TmAdd(t1, t2) -> print_string "(add ";
                     pretty_printer t1;
                      print_char ' ';
                     pretty_printer t2;
                     print_string ")";
  | TmMul(t1, t2) -> print_string "(mul ";
                      pretty_printer t1;
                      print_char ' ';
                      pretty_printer t2;
                      print_string ")";
  | TmSub(t1, t2) -> print_string "(sub ";
                      pretty_printer t1;
                      print_char ' ';
                      pretty_printer t2;
                      print_string ")";
  | TmEq(t1, t2) -> print_string "(eq ";
                      pretty_printer t1;
                      print_char ' ';
                      pretty_printer t2;
                      print_string ")";
  | TmTrue            -> print_string "true"
  | TmFalse           -> print_string "false"
  | TmIf(t1, t2, t3)  -> print_string "(if ";
                        pretty_printer t1;
                        print_string " then ";
                        pretty_printer t2;
                        print_string " else ";
                        pretty_printer t3;
                        print_string ")";
  | TmPair(t1, t2)    -> print_string "(";
                         pretty_printer t1;
                         print_string ",";
                         pretty_printer t2;
                         print_string ")";
  | TmFix(t1)         -> print_string "(fix ";
                         pretty_printer t1;
                         print_string ")"
  | _   -> print_string "???"
                      

(*                         
  | TmFst of term
  | TmSnd of term

  | TmNil
  | TmCons of term * term
  | TmHead of term
  | TmTail of term
  | TmIsNil of term *)

let ctrue  = TmAbs("x", TmAbs("y", TmVar("x")))
let cfalse = TmAbs("x", TmAbs("y", TmVar("y")))

let cand   = TmAbs("x", TmAbs("y",
              TmApp(
                TmApp(TmVar("x"), TmVar("y")),
                TmVar("x")
              )  
            ))

let cif = TmAbs("p", TmAbs("a", TmAbs("b",
          TmApp(
            TmApp(TmVar("p"), TmVar("a")),
            TmVar("b")
          )
        )))


let czero  = TmAbs("s", TmAbs("z", TmVar("z")))

let csucc  = TmAbs("n", TmAbs("s", TmAbs("z", TmApp(
              TmVar("s"),
              TmApp(
                TmApp(TmVar("n"), TmVar("s")),
                TmVar("z")
              )
            ))))
let cadd = TmAbs("m", TmAbs("n", TmAbs("s", TmAbs("z",
            TmApp(
              TmApp(TmVar("m"), TmVar("s")),
              TmApp(
                  TmApp(TmVar("n"), TmVar("s")),
                  TmVar("z")
              )
            )
          ))))
let cmul = TmAbs("m", TmAbs("n", TmAbs("s", TmAbs("z",
            TmApp(
              TmApp(
                TmVar("m"),
                TmApp(TmVar("n"), TmVar("s"))
              ),
              TmVar("z")
            )
          ))))
let cpred = TmAbs("n", TmAbs("s", TmAbs("z",
              TmApp(
                TmApp(
                  TmApp(
                    TmVar("n"),
                    TmAbs("g", TmAbs("h",
                      TmApp(
                        TmVar("h"),
                        TmApp(TmVar("g"), TmVar("s"))
                      )
                    ))
                  ),
                  TmAbs("u", TmVar("z"))
                ),
                TmAbs("u", TmVar("u"))
              )
            )))
let csub = TmAbs("m", TmAbs("n",
            TmApp(
              TmApp(TmVar("n"), cpred),
              TmVar("m")
            )
          ))
let iszero = TmAbs("n",
              TmApp(
                TmApp(
                  TmVar("n"),
                  TmAbs("x", cfalse)
                ),
                ctrue
              )
            )
let isleq = TmAbs("m", TmAbs("n",
            TmApp(
              iszero,
              TmApp(
                TmApp(csub, TmVar("m")),
                TmVar("n")
              )
            )
          ))

let iseq = TmAbs("m", TmAbs("n",
            TmApp(
              TmApp(
                cand,
                TmApp(TmApp(isleq, TmVar("m")), TmVar("n"))
              ),
              TmApp(TmApp(isleq, TmVar("n")), TmVar("m"))
            )
          ))


let fix_foo1 = TmAbs("y",
                TmApp(
                  TmApp(TmVar("x"), TmVar("x")),
                  TmVar("y")
                )
              )

let fix_foo2 = TmAbs("x", TmApp(TmVar("f"), fix_foo1))

let fix = TmAbs("f", TmApp(fix_foo2, fix_foo2))

let cnum n =
  let rec createSs n =
    if n=0 then TmVar("z") else
    TmApp(TmVar("s"), createSs (n-1)) in
  TmAbs("s", TmAbs("z", createSs n))

let app_helper1 f x = TmApp(f, x)
let app_helper2 f x y = TmApp(TmApp(f, x), y)
let app_helper3 f x y z = TmApp(TmApp(TmApp(f, x), y), z)

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
  | _ as other-> other

let main () =
  let cin =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel cin in
    let res = try Parser.toplevel Lexer.main lexbuf
    with Parsing.Parse_error -> print_string "Parse error"; failwith "a"
    in pretty_printer res; print_char '\n';
    pretty_printer (remove_sugar res); print_char '\n'
let _ = main ()