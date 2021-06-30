open Ast
open Env

let rec print = function
  | Var k -> k
  | IntLit l -> string_of_int l
  | BoolLit b -> string_of_bool b
  | BinaryOp (op, l, r) -> print_binary_op op l r
and print_binary_op op l r = match op with
  | Add -> Printf.sprintf "(+ %s %s)" (print l) (print r)
  | Sub -> Printf.sprintf "(- %s %s)" (print l) (print r)
  | Mul -> Printf.sprintf "(* %s %s)" (print l) (print r)
  | Div -> Printf.sprintf "(/ %s %s)" (print l) (print r)
  | Lt -> Printf.sprintf "(< %s %s)" (print l) (print r)

let rec eval env = function
  | Var k -> lookup k env
  | IntLit l -> IntLit l
  | BoolLit b -> BoolLit b
  | BinaryOp (op, l, r) -> 
    let l = eval env l in
    let r = eval env r in
    eval_binary_op op l r
and eval_binary_op op l r = match op, l, r with
  | Add, IntLit l, IntLit r -> IntLit (l + r)
  | Add, _, _ -> err ("Both args must be integer: +")
  | Sub, IntLit l, IntLit r -> IntLit (l - r)
  | Sub, _, _ -> err ("Both args must be integer: -")
  | Mul, IntLit l, IntLit r -> IntLit (l * r)
  | Mul, _, _ -> err ("Both args must be integer: *")
  | Div, IntLit l, IntLit r -> IntLit (l / r)
  | Div, _, _ -> err ("Both args must be integer: /")
  | Lt, IntLit l, IntLit r  -> BoolLit (l < r)
  | Lt, _, _ -> err ("Both args must be integer: <")

let eval_decl env = function
  | ExprStmt e -> let v = eval env e in ("_", env, v)
  | LetStmt (k,e) -> 
    let v = eval env e in 
    let new_env = extend k v env in
    (k, new_env, v)


let rec print_program env = function
  | [] -> ()
  | stmt::stmt_list ->
    let (k, new_env, v) = eval_decl env stmt in
    Printf.printf "%s = %s\n" k (print v);
    print_program new_env stmt_list