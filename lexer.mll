(* 
   The lexical analyzer: lexer.ml is generated automatically
   from lexer.mll.
   
   The only modification commonly needed here is adding new keywords to the 
   list of reserved words at the top.  
*)

{

let reservedWords = [
  (* Keywords *)
  ("lambda", Parser.LAMBDA);
  ("λ", Parser.LAMBDA);
  ("add", Parser.ADD);
  ("mul", Parser.MUL);
  ("sub", Parser.SUB);
  ("eq", Parser.EQ);
  ("=", Parser.EQ);
  ("true", Parser.TRUE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("else", Parser.ELSE);
  ("fix", Parser.FIX);
  ("pair", Parser.PAIR);
  ("fst", Parser.FST);
  ("snd", Parser.SND);
  ("nil", Parser.NIL);
  ("cons", Parser.CONS);
  ("head", Parser.HEAD);
  ("tail", Parser.TAIL);
  ("isnil", Parser.ISNIL);

  (* Symbols *)
  (".", Parser.DOT);
  (",", Parser.COMMA);
  ("(", Parser.LPAREN);
  (")", Parser.RPAREN);
]

(* Support functions *)

let (symbolTable : (string, Parser.token) Hashtbl.t) = Hashtbl.create 1024
let _ =
  List.iter (fun (key, data) -> Hashtbl.add symbolTable key data) reservedWords

let createID str =  
  try
    Hashtbl.find symbolTable str
  with Not_found ->
    Parser.VAR str

let filename = ref ""

let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := Filename.concat (Sys.getcwd()) inFile;
  Lexing.from_channel stream

let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))
}

let newline = '\n'
let char = [ 'A'-'Z' 'a'-'z' ]
let special_char = [ '.' ',' '(' ')' '=' ] | "λ"
let comment_line = "//"([^ '\n' ]+)
let number = ['0'-'9']+
let space = [' ' '\009' '\012']

(* The main body of the lexical analyzer *)

rule main = parse
| eof                  { Parser.EOF }
| space+               { main lexbuf }
| space*newline        { main lexbuf }
| "/*"                 { comment lexbuf }
| comment_line         { main lexbuf }
| special_char as c    { createID c }
| char+ as iden        { createID iden }
| number as num        { Parser.NUM (int_of_string num) }
| _                    { print_string "fail"; failwith "illegal character" }

and comment = parse
| "*/"  { main lexbuf }
| _     { comment lexbuf }
| eof   { failwith "Comment not terminated" }

(*  *)