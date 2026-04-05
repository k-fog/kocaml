type expr = { desc : expr_desc; span : Span.t }

and expr_desc =
  | Int of int
  | Bool of bool
  | Var of string
  | BinExpr of Ast.binop * expr * expr
  | UnaryExpr of Ast.unop * expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Closure of closure
  | AppClosure of expr * expr list
  | AppDirect of string * expr list
  | EnvGet of string

and closure = { entry : string; actual_fv : string list }

type fundef = {
  name : string;
  params : string list;
  fv : string list;
  body : expr;
}

type program = { fundefs : fundef list; main : expr }

module StringSet = Set.Make (String)

let free_vars ast =
  let rec aux bound ast =
    let open Ast in
    match ast.desc with
    | Int _ | Bool _ -> StringSet.empty
    | Var var ->
        if StringSet.mem var bound then StringSet.empty
        else StringSet.singleton var
    | BinExpr (_, lhs, rhs) -> StringSet.union (aux bound lhs) (aux bound rhs)
    | UnaryExpr (_, rhs) -> aux bound rhs
    | And (lhs, rhs) | Or (lhs, rhs) ->
        StringSet.union (aux bound lhs) (aux bound rhs)
    | If (e1, e2, e3) ->
        StringSet.union (aux bound e1)
          (StringSet.union (aux bound e2) (aux bound e3))
    | Let (var, e1, e2) ->
        StringSet.union (aux bound e1) (aux (StringSet.add var bound) e2)
    | LetRec (var, param, e1, e2) ->
        let s2 = StringSet.add var bound in
        let s1 = StringSet.add param s2 in
        StringSet.union (aux s1 e1) (aux s2 e2)
    | Fun (param, body) -> aux (StringSet.add param bound) body
    | App (e1, e2) -> StringSet.union (aux bound e1) (aux bound e2)
  in
  aux StringSet.empty ast

(* Given local variables and an AST, returns the closure-converted AST. *)
let convert ast =
  let funcs = ref [] in
  let rec aux local ast =
    let desc =
      match (ast : Ast.expr).desc with
      | Ast.Int n -> Int n
      | Ast.Bool b -> Bool b
      | Ast.Var var -> if StringSet.mem var local then Var var else EnvGet var
      | Ast.BinExpr (op, lhs, rhs) -> BinExpr (op, aux local lhs, aux local rhs)
      | Ast.UnaryExpr (op, rhs) -> UnaryExpr (op, aux local rhs)
      | Ast.And (lhs, rhs) -> And (aux local lhs, aux local rhs)
      | Ast.Or (lhs, rhs) -> Or (aux local lhs, aux local rhs)
      | Ast.If (e1, e2, e3) -> If (aux local e1, aux local e2, aux local e3)
      | Ast.Let (var, e1, e2) ->
          let local' = StringSet.add var local in
          Let (var, aux local e1, aux local' e2)
      | Ast.LetRec (f, x, body, rest) -> failwith "TODO"
      | Ast.Fun (param, body) -> failwith "TODO"
      | Ast.App (f, x) -> failwith "TODO"
    in
    { desc; span = ast.span }
  in
  let ast' = aux StringSet.empty ast in
  { fundefs = !funcs; main = ast' }
