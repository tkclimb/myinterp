%token LET IN
%token REC
%token IF THEN ELSE
%token FUN
%token RARROW
%token LPAREN RPAREN
%token PLUS MINUS
%token STAR SLASH
%token EQ LT GT
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
  | LET k=ID EQ e=expr SEMICOLON { Ast.LetStmt (k, e) }
  | LET REC k=ID EQ FUN p=ID RARROW e=expr SEMICOLON {
      Ast.LetRecStmt (k, p, e)
    }

expr:
  | e=cmp_expr { e }
  | e=let_expr { e }
  | e=let_rec_expr { e }
  | e=if_expr  { e }
  | e=fun_expr { e }

let_expr: LET k=ID EQ e1=expr IN e2=expr {
  Ast.LetExpr (k, e1, e2)
}

let_rec_expr: LET REC k=ID EQ FUN p=ID RARROW e1=expr IN e2=expr {
  Ast.LetRecExpr (k, p, e1, e2)
}

if_expr: IF c=expr THEN et=expr ELSE ef=expr {
  Ast.IfExpr (c, et, ef)
}

fun_expr: FUN k=ID RARROW e=expr {
  Ast.FunExpr (k, e)
}

cmp_expr:
  | l=cmp_expr EQ r=add_expr { Ast.BinOpExpr (Ast.Eq, l, r) }
  | l=cmp_expr LT r=add_expr { Ast.BinOpExpr (Ast.Lt, l, r) }
  | l=cmp_expr GT r=add_expr { Ast.BinOpExpr (Ast.Gt, l, r) }
  | e=add_expr { e }

add_expr:
  | l=add_expr PLUS r=mul_expr { Ast.BinOpExpr (Ast.Add, l, r) }
  | l=add_expr MINUS r=mul_expr { Ast.BinOpExpr (Ast.Sub, l, r) }
  | e=mul_expr { e }

mul_expr:
  | l=mul_expr STAR r=apply_expr { Ast.BinOpExpr (Ast.Mul, l, r) }
  | l=mul_expr SLASH r=apply_expr { Ast.BinOpExpr (Ast.Div, l, r) }
  | e=apply_expr { e }

apply_expr:
  | e1=apply_expr e2=primary_expr { Ast.AppExpr (e1, e2) }
  | e=primary_expr { e }

primary_expr:
  | k=ID { Ast.Var k }
  | i=INT_LIT { Ast.IntLit i }
  | b=BOOL_LIT { Ast.BoolLit b }
  // | LPAREN PLUS RPAREN { Ast.FunExpr (, ) }
  | LPAREN e=expr RPAREN { e }