open Ast

let rec eq a_typ b_typ =
  match (a_typ, b_typ) with
  | TInt, TInt -> true
  | (TAbs (param_a, body_a), TAbs (param_b, body_b)) -> eq param_a param_b && eq body_a body_b
  | _ -> false

let rec to_string typ =
  match typ with
  | TInt -> "int"
  | TAbs (param, body) -> Printf.sprintf "(%s -> %s)" (to_string param) (to_string body)

let rec infer context expr =
  match expr with
  | Ast.Int _int -> TInt
  | Ast.Var name ->
    (match Context.find_opt name context with
    | Some typ -> typ
    | None -> failwith "type_error: variable not found")
  | Ast.Abs (param, param_typ, body) ->
    let context = Context.add param param_typ context in
    let b_typ = infer context body in
    TAbs(param_typ, b_typ)
  | Ast.App (l_expr, r_expr) ->
      (match infer context l_expr with
      | TAbs (t_arg, t_body) ->
          let t_param = infer context r_expr in
          (match eq t_arg t_param with
          | true -> t_body
          | false -> failwith (Printf.sprintf "type_error: this expression has type %s but an expression was expected of type %s" (to_string t_arg) (to_string t_param)))
      | TInt -> failwith "type_error: expecting abs got int")
