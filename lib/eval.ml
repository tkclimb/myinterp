open Ast
open Env

let rec string_of_expr = function
  | Var k -> k
  | IntLit l -> string_of_int l
  | BoolLit b -> string_of_bool b
  | BinOpExpr (op, l, r) -> (match op with
    | Add -> Printf.sprintf "(+ %s %s)" (string_of_expr l) (string_of_expr r)
    | Sub -> Printf.sprintf "(- %s %s)" (string_of_expr l) (string_of_expr r)
    | Mul -> Printf.sprintf "(* %s %s)" (string_of_expr l) (string_of_expr r)
    | Div -> Printf.sprintf "(/ %s %s)" (string_of_expr l) (string_of_expr r)
    | Eq  -> Printf.sprintf "(= %s %s)" (string_of_expr l) (string_of_expr r)
    | Lt  -> Printf.sprintf "(< %s %s)" (string_of_expr l) (string_of_expr r)
    | Gt  -> Printf.sprintf "(> %s %s)" (string_of_expr l) (string_of_expr r))
  | LetExpr (k, e1, e2) -> (Printf.printf
    "let %s = %s in " k (string_of_expr e1);
    string_of_expr e2)
  | LetRecExpr (k, p, e1, e2) -> (Printf.printf
    "let rec %s = fun %s -> %s in " k p (string_of_expr e1);
    string_of_expr e2)
  | IfExpr (c, et, ef) -> (Printf.sprintf 
    "if %s then %s else %s" (string_of_expr c) (string_of_expr et) (string_of_expr ef))
  | FunExpr (k, e) -> (Printf.sprintf
    "fn %s -> %s" k (string_of_expr e))
  | AppExpr (e1, e2) -> (Printf.sprintf
    "%s(%s)" (string_of_expr e1) (string_of_expr e2))
let string_of_stmt = function
  | ExprStmt e -> string_of_expr e
  | LetStmt (k, e) -> (Printf.sprintf
    "let %s = %s" k (string_of_expr e))
  | LetRecStmt (k, p, e) -> (Printf.sprintf
    "let rec %s = fun %s -> %s" k p (string_of_expr e))

let string_of_var = function
  | IntVar l -> string_of_int l
  | BoolVar b -> string_of_bool b
  | ClosureVar (k, e, _) -> (Printf.sprintf
    "(%s) -> %s" k (string_of_expr e))

let rec eval_expr env = function
  | Var k -> lookup k env
  | IntLit l -> IntVar l
  | BoolLit b -> BoolVar b
  | BinOpExpr (op, l, r) -> eval_binary_op env op l r
  | LetExpr (k, e1, e2) -> eval_let_expr env k e1 e2 
  | LetRecExpr (k, p, e1, e2) -> eval_let_rec_expr env k p e1 e2 
  | IfExpr (c, et, ef) -> eval_if_expr env c et ef
  | FunExpr (k, e) -> eval_fun_expr env k e
  | AppExpr (e1, e2) -> eval_apply_expr env e1 e2
and eval_binary_op env op l r = 
  let l = eval_expr env l in
  let r = eval_expr env r in
  match op, l, r with
    | Add, IntVar l, IntVar r -> IntVar (l + r)
    | Add, _, _ -> err ("Both args must be integer: +")
    | Sub, IntVar l, IntVar r -> IntVar (l - r)
    | Sub, _, _ -> err ("Both args must be integer: -")
    | Mul, IntVar l, IntVar r -> IntVar (l * r)
    | Mul, _, _ -> err ("Both args must be integer: *")
    | Div, IntVar l, IntVar r -> IntVar (l / r)
    | Div, _, _ -> err ("Both args must be integer: /")
    | Eq, IntVar l, IntVar r -> BoolVar (l = r)
    | Eq, _, _ -> err ("Both args must be integer: <")
    | Lt, IntVar l, IntVar r -> BoolVar (l < r)
    | Lt, _, _ -> err ("Both args must be integer: <")
    | Gt, IntVar l, IntVar r -> BoolVar (l > r)
    | Gt, _, _ -> err ("Both args must be integer: <")
and eval_let_expr env k e1 e2 =
  let v = eval_expr env e1 in 
  let new_env = extend k v env in
  eval_expr new_env e2
and eval_let_rec_expr env k p e1 e2 =
  let tmp_env = ref Env.empty in
  let tmp_closure = ClosureVar (p, e1, tmp_env) in
  let new_env = Env.extend k tmp_closure env in
  tmp_env := new_env;
  eval_expr new_env e2
and eval_if_expr env c et ef =
  let v = eval_expr env c in 
  match v with
  | BoolVar b ->
    if b then (eval_expr env et)
    else (eval_expr env ef)
  | _ -> err ("Invalid condition found...")
and eval_fun_expr env k e = 
  let ref_env = ref env in
  ClosureVar (k, e, ref_env)
and eval_apply_expr env e1 e2 = 
  let caller = eval_expr env e1 in
  let callee = eval_expr env e2 in
  match caller with
  | ClosureVar (k, e, bind_env) ->
    let new_env = extend k callee !bind_env in
    eval_expr new_env e
  | _ -> err ("Invalid function applied...")

let eval_stmt env = function
  | ExprStmt e -> let v = eval_expr env e in ("_", env, v)
  | LetStmt (k, e) -> let v = eval_expr env e in (k, env, v)
  | LetRecStmt (k, p, e) ->
    let tmp_env = ref Env.empty in
    let tmp_closure = ClosureVar (p, e, tmp_env) in
    let new_env = Env.extend k tmp_closure env in
    tmp_env := new_env;
    (k, env, tmp_closure)


let rec print_program env = function
  | [] -> ()
  | stmt::stmt_list ->
    Printf.printf "%s\n" (string_of_stmt stmt);
    let (k, new_env, v) = eval_stmt env stmt in
    Printf.printf "%s = %s\n" k (string_of_var v);
    print_program new_env stmt_list