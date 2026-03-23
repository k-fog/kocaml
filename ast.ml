type binop = Add | Sub | Mul | Div

type expr_desc =
  | Int of int
  | Var of string
  | BinExpr of binop * expr * expr
  | Neg of expr
  | Let of string * expr * expr

and expr = { desc : expr_desc; span : Span.t }

let int n span = { desc = Int n; span }
let var v span = { desc = Var v; span }

let bin_expr op lhs rhs =
  { desc = BinExpr (op, lhs, rhs); span = Span.merge lhs.span rhs.span }

let neg n span = { desc = Neg n; span }
let let_expr var e1 e2 span = { desc = Let (var, e1, e2); span }
