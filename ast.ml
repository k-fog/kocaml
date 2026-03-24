type binop = Add | Sub | Mul | Div | Lt | Le | Eq | Neq

type expr_desc =
  | Int of int
  | Bool of bool
  | Var of string
  | BinExpr of binop * expr * expr
  | Neg of expr
  | Let of string * expr * expr
  | LetRec of string * string * expr * expr (* var param body rest *)
  | Fun of string * expr
  | App of expr * expr
  | If of expr * expr * expr

and expr = { desc : expr_desc; span : Span.t }

let with_span e span = { e with span }
let int n span = { desc = Int n; span }
let bool b span = { desc = Bool b; span }
let var v span = { desc = Var v; span }

let bin_expr op lhs rhs =
  { desc = BinExpr (op, lhs, rhs); span = Span.merge lhs.span rhs.span }

let neg n span = { desc = Neg n; span }
let let_expr var e1 e2 span = { desc = Let (var, e1, e2); span }
let letrec var param e1 e2 span = { desc = LetRec (var, param, e1, e2); span }
let fun_expr var e span = { desc = Fun (var, e); span }
let app fn arg = { desc = App (fn, arg); span = Span.merge fn.span arg.span }
let if_expr cond e1 e2 span = { desc = If (cond, e1, e2); span }
