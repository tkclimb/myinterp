open Myinterp

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prog = Parser.program Lexer.tokenize lexbuf in
  Eval.print_program Env.empty prog