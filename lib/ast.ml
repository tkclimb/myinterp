exception Error of string
let err s = raise (Error s)

type id = string

type bin_op = Add | Sub | Mul | Div | Lt

type expr = 
  | IntLit of int
  | BoolLit of bool
  | BinaryOp of bin_op * expr * expr

type stmt = 
  | ExprStmt of expr
  | LetStmt of id * expr

type program = stmt list

let rec print = function
  | IntLit l -> string_of_int l
  | BoolLit b -> string_of_bool b
  | BinaryOp (op, l, r) -> print_binary_op op l r
and print_binary_op op l r = match op with
  | Add -> Printf.sprintf "(+ %s %s)" (print l) (print r)
  | Sub -> Printf.sprintf "(- %s %s)" (print l) (print r)
  | Mul -> Printf.sprintf "(* %s %s)" (print l) (print r)
  | Div -> Printf.sprintf "(/ %s %s)" (print l) (print r)
  | Lt -> Printf.sprintf "(< %s %s)" (print l) (print r)

let rec eval = function
  | IntLit l -> IntLit l
  | BoolLit b -> BoolLit b
  | BinaryOp (op, l, r) -> 
    let l = eval l in
    let r = eval r in
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


let rec print_program = function
  | [] -> ()
  | stmt::stmt_list ->
      match stmt with
        | ExprStmt (expr) ->
          let result = eval expr in
          Printf.printf "%s = %s\n" (print expr) (print result)
        | LetStmt (id, expr) ->
          let result = eval expr in
          Printf.printf "%s = %s\n" id (print result);
      print_program stmt_list