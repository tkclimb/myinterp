{
open Parser
}

let digit = ['0'-'9']
let id_string = ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
let number = '-'? digit digit*
let whitespace = ['\t' ' ' '\n']
let comment_out = "(*" (_*) "*)"

rule tokenize = parse
  | comment_out { tokenize lexbuf }
  | whitespace+ { tokenize lexbuf }
  | "let"   { LET }
  | "in"    { IN }
  | "rec"   { REC }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "fun"   { FUN }
  | "->"    { RARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "=" { EQ }
  | "<" { LT }
  | ">" { GT }
  | ";" { SEMICOLON }
  | "true"  { BOOL_LIT true }
  | "false" { BOOL_LIT false }
  | id_string as id { ID id }
  | number as n { INT_LIT (int_of_string n ) }
  | eof { EOF }