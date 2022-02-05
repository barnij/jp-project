/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token LAMBDA
%token ADD
%token MUL
%token SUB
%token EQ
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token FIX
%token PAIR
%token FST
%token SND
%token NIL
%token CONS
%token HEAD
%token TAIL
%token ISNIL

/* Identifier and constant value tokens */
%token <string> VAR
%token <int> NUM

/* Symbolic tokens */
%token COMMA
%token DOT
%token EOF
%token LPAREN
%token RPAREN


/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Syntax.term > toplevel
%type < Syntax.term > term
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    | term EOF
        { $1 }

term :
    | app_term
        { $1 }
    | LAMBDA x=VAR DOT t=term
        { TmAbs(x, t) }
    | IF t1=term THEN t2=term ELSE t3=term
        { TmIf(t1, t2, t3) }
    | LPAREN; t1=term; COMMA; t2=term; RPAREN
        { TmPair(t1, t2) }

app_term:
    | aterm
        { $1 }
    | t1=app_term t2=aterm
        { TmApp(t1, t2) }
    | PAIR; t1=aterm; t2=aterm
        { TmPair(t1, t2) }
    | ADD; t1=aterm; t2=aterm
        { TmAdd(t1, t2) }
    | MUL; t1=aterm; t2=aterm
        { TmMul(t1, t2) }
    | SUB; t1=aterm; t2=aterm
        { TmSub(t1, t2) }
    | EQ; t1=aterm; t2=aterm
        { TmEq(t1, t2) }
    | FIX; t=aterm
        { TmFix(t) }
    | FST; t=aterm
        { TmFst(t) }
    | SND; t=aterm
        { TmSnd(t) }
    | CONS; t1=aterm; t2=aterm
        { TmCons(t1, t2) }
    | HEAD; t=aterm
        { TmHead(t) }
    | TAIL; t=aterm
        { TmTail(t) }
    | ISNIL; t=aterm
        { TmIsNil(t) }

aterm:
    | LPAREN t=term RPAREN
        { t }
    | x=VAR
        { TmVar(x) }
    | n=NUM
        { TmNum(n) }
    | TRUE
        { TmTrue }
    | FALSE
        { TmFalse }
    | NIL
        { TmNil }

/*   */
