type binop = Add | Sub | Mul | Div
type expr = Int of int | BinExpr of binop * expr * expr | Neg of expr
