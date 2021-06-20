open Myinterp

let rec print_result = function
  | [] -> ()
  | stmt::stmt_list ->
      match stmt with
        Ast.ExprStmt (expr) ->
          let result = Ast.eval expr in
          Printf.printf "%s = %s\n" (Ast.print expr) (Ast.print result);
      print_result stmt_list

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prog = Parser.program Lexer.tokenize lexbuf in
  print_result prog