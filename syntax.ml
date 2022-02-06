open Format

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

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

type command =
  | Eval of term
  | Bind of string

type 'a env = 'a list

type mvalue =
  | Clo of term * mvalue env

type stack =
  | E_mt
  | E_arg of term * mvalue env * stack


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

let pair = 
  TmAbs("a", TmAbs("b", TmAbs("f",
    TmApp(
      TmApp(TmVar("f"), TmVar("a")),
      TmVar("b")
  ))))

let pair_fst = ctrue
let pair_snd = cfalse

let app_helper1 f x = TmApp(f, x)
let app_helper2 f x y = TmApp(TmApp(f, x), y)
let app_helper3 f x y z = TmApp(TmApp(TmApp(f, x), y), z)

let cnil = czero
let ccons = 
  TmAbs("h", TmAbs("t", TmAbs("f",
    TmApp(
      TmApp(TmVar("f"), TmVar("h")),
      TmVar("t")
    )
  )))
let chead = pair_fst
let ctail = pair_snd
let isnil = iszero

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


