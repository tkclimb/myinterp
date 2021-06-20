{
open Parser
}

let digit = ['0'-'9']
let id_string = ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
let number = '-'? digit digit*
let whitespace = ['\t' ' ' '\n']

rule tokenize = parse
  | whitespace+ { tokenize lexbuf }
  | "let"   { LET }
  | "true"  { BOOL_LIT true }
  | "false" { BOOL_LIT false }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "<" { LT }
  | "=" { ASSIGN }
  | ";" { SEMICOLON }
  | id_string as id { ID id }
  | number as n { INT_LIT (int_of_string n ) }
  | eof { EOF }