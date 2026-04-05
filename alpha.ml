module StringMap = Map.Make (String)

let fresh var = Printf.sprintf "%s.%d" var (Id.gen ())
let restore var = String.split_on_char '.' var |> List.hd

let convert ast =
  let rec aux ast env =
    let open Ast in
    match ast.desc with
    | Int _ | Bool _ -> ast
    | Var var ->
        if StringMap.mem var env then
          { ast with desc = Var (StringMap.find var env) }
        else ast
    | BinExpr (op, lhs, rhs) ->
        { ast with desc = BinExpr (op, aux lhs env, aux rhs env) }
    | UnaryExpr (op, rhs) -> { ast with desc = UnaryExpr (op, aux rhs env) }
    | And (lhs, rhs) -> { ast with desc = And (aux lhs env, aux rhs env) }
    | Or (lhs, rhs) -> { ast with desc = Or (aux lhs env, aux rhs env) }
    | If (e1, e2, e3) ->
        { ast with desc = If (aux e1 env, aux e2 env, aux e3 env) }
    | Let (var, e1, e2) ->
        let var' = fresh var in
        let env' = StringMap.add var var' env in
        let e1' = aux e1 env in
        let e2' = aux e2 env' in
        { ast with desc = Let (var', e1', e2') }
    | LetRec (f, param, e1, e2) ->
        let f' = fresh f in
        let param' = fresh param in
        let env2 = StringMap.add f f' env in
        let env1 = StringMap.add param param' env2 in
        { ast with desc = LetRec (f', param', aux e1 env1, aux e2 env2) }
    | Fun (param, e) ->
        let param' = fresh param in
        let env' = StringMap.add param param' env in
        { ast with desc = Fun (param', aux e env') }
    | App (e1, e2) -> { ast with desc = App (aux e1 env, aux e2 env) }
  in
  aux ast StringMap.empty
