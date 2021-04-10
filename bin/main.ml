open Myinterp

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Lexer.tokenize lexbuf in
  let result = Ast.eval expr in
  Printf.printf "%s = %s\n" (Ast.print expr) (Ast.print result)