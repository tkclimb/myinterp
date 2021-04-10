{
open Parser
}

let digit = ['0'-'9']
let number = '-'? digit digit*
let whitespace = ['\t' ' ' '\n']

rule tokenize = parse
  | whitespace+ { tokenize lexbuf }
  | number as n { INT_LIT (int_of_string n ) }
  | "true"  { BOOL_LIT true }
  | "false" { BOOL_LIT false }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "<" { LT }
  | eof { EOF }