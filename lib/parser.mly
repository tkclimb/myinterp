%token LET IN
%token IF THEN ELSE
%token LPAREN RPAREN
%token PLUS MINUS
%token STAR SLASH
%token LT
%token ASSIGN
%token SEMICOLON
%token EOF

%token <string> ID
%token <int> INT_LIT
%token <bool> BOOL_LIT

%start <Ast.program> program

%%

program: sl=stmt_list EOF { Utils.rev_list sl }

stmt_list:
  | s=stmt { s::[] }
  | b=stmt_list s=stmt { s::b }

stmt:
  | e=expr SEMICOLON { Ast.ExprStmt e }

expr:
  | e=cmp_expr { e }
  | e=let_expr { e }
  | e=if_expr  { e }

let_expr: LET v=ID ASSIGN e1=expr IN e2=expr {
  Ast.LetExpr (v, e1, e2)
}

if_expr: IF c=expr THEN et=expr ELSE ef=expr {
  Ast.IfExpr (c, et, ef)
}

cmp_expr:
  | l=cmp_expr LT r=add_expr { Ast.BinaryOp (Ast.Lt, l, r) }
  | e=add_expr { e }

add_expr:
  | l=add_expr PLUS r=mul_expr { Ast.BinaryOp (Ast.Add, l, r) }
  | l=add_expr MINUS r=mul_expr { Ast.BinaryOp (Ast.Sub, l, r) }
  | e=mul_expr { e }

mul_expr:
  | l=mul_expr STAR r=primary_expr { Ast.BinaryOp (Ast.Mul, l, r) }
  | l=mul_expr SLASH r=primary_expr { Ast.BinaryOp (Ast.Div, l, r) }
  | e=primary_expr { e }

primary_expr:
  | k=ID { Ast.Var k }
  | i=INT_LIT { Ast.IntLit i }
  | b=BOOL_LIT { Ast.BoolLit b }
  | LPAREN e=expr RPAREN { e }