type binop = Add | Sub | Mul | Div

type expr_desc = Int of int | BinExpr of binop * expr * expr | Neg of expr
and expr = { desc : expr_desc; span : Span.t }

let int n span = { desc = Int n; span }

let bin_expr op lhs rhs =
  { desc = BinExpr (op, lhs, rhs); span = Span.merge lhs.span rhs.span }

let neg n span = { desc = Neg n; span }
