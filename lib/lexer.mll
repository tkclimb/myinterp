{
open Parser
}

let digit = ['0'-'9']
let id_string = ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
let number = '-'? digit digit*
let whitespace = ['\t' ' ' '\n']
let comment_out = '#' ([^'\n']*)

rule tokenize = parse
  | comment_out { tokenize lexbuf }
  | whitespace+ { tokenize lexbuf }
  | "let"   { LET }
  | "in"    { IN }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "<" { LT }
  | "=" { ASSIGN }
  | ";" { SEMICOLON }
  | "true"  { BOOL_LIT true }
  | "false" { BOOL_LIT false }
  | id_string as id { ID id }
  | number as n { INT_LIT (int_of_string n ) }
  | eof { EOF }