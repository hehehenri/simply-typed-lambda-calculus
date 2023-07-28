module Context = Map.Make(String)

type expr =
  | Int of int
  | Var of string
  | Abs of string * expr
  | App of expr * expr

type value =
  | Int of int
  | Closure of string * expr * value Context.t
  | Native of (value -> value)

module Infer = struct
  type typ =
    | TAbs of typ * typ
    | TInt

  let rec eq a_typ b_typ =
    match (a_typ, b_typ) with
    | TInt, TInt -> true
    | (TAbs (param_a, body_a), TAbs (param_b, body_b)) -> eq param_a param_b && eq body_a body_b
    | _ -> false

  let rec to_string typ =
    match typ with
    | TInt -> "int"
    | TAbs (param, body) -> Printf.sprintf "(%s -> %s)" (to_string param) (to_string body)

  let rec infer context (expr:expr) =
    match expr with
    | Int _int -> TInt
    | Var name ->
      (match Context.find name context with
      | Some typ -> typ
      | None -> failwith "type_error: variable not found")
    | Abs (param, body) ->
      (match Context.find param context with
        | Some(t_param) -> let t_body = infer context body in
          TAbs (t_param, t_body)
        | None -> failwith "type_error: variable not found")
    | App (l_expr, r_expr) ->
        (match infer context l_expr with
        | TAbs (t_arg, t_body) ->
            let t_param = infer context r_expr in
            (match eq t_arg t_param with
            | true -> t_body
            | false -> failwith (Printf.sprintf "type_error: expecting %s got %s" (to_string t_arg) (to_string t_param)))
        | TInt -> failwith "type_error: expecting abs got int")
end

let rec eval context expr =
  let apply context l_expr r_expr =
    let arg = eval context r_expr in
      match eval context l_expr with
      | Closure (param, body, context) ->
          let context = Context.add param arg context in
          eval context body 
      | Native value -> value arg 
      | Int _ -> assert false in

  match expr with
  | Var name -> (match Context.mem name context with
    | true -> Context.find name context
    | false -> failwith ("unbound variable: " ^ name))
  | Abs (param, body) -> Closure (param, body, context)
  | Int number -> Int number
  | App (l_expr, r_expr) -> apply context l_expr r_expr

let rec to_string expr =
  match expr with
  | Var name -> Printf.sprintf "Var %s" name
  | Abs (arg, body) -> Printf.sprintf "Abs(%s, %s)" arg (to_string body)
  | App (l_expr, r_expr) -> Printf.sprintf "App(%s, %s)" (to_string l_expr) (to_string r_expr)
  | Int number -> Printf.sprintf "Int %d" number

