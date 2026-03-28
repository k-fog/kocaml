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
    | Let (var, e1, e2) ->
        StringSet.union (aux bound e1) (aux (StringSet.add var bound) e2)
    | LetRec (var, param, e1, e2) ->
        let s2 = StringSet.add var bound in
        let s1 = StringSet.add param s2 in
        StringSet.union (aux s1 e1) (aux s2 e2)
    | Fun (param, e) -> aux (StringSet.add param bound) e
    | App (e1, e2) -> StringSet.union (aux bound e1) (aux bound e2)
    | If (e1, e2, e3) ->
        StringSet.union (aux bound e1)
          (StringSet.union (aux bound e2) (aux bound e3))
  in
  aux StringSet.empty ast |> StringSet.elements
