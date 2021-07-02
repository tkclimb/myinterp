exception Error of string
let err s = raise (Error s)

type id = string

type bin_op = Add | Sub | Mul | Div | Lt

type expr = 
  | Var of id
  | IntLit of int
  | BoolLit of bool
  | BinOpExpr of bin_op * expr * expr
  | LetExpr of id * expr * expr
  | IfExpr of expr * expr * expr
  | FunExpr of id * expr
  | AppExpr of expr * expr

type var = 
  | IntVar of int
  | BoolVar of bool
  | ClosureVar of id * expr * var Env.t

type stmt = 
  | ExprStmt of expr

type program = stmt list
