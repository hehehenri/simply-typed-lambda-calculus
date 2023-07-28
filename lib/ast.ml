module Context = Map.Make(String)

type typ =
  | TAbs of typ * typ
  | TInt

type expr =
  | Int of int
  | Var of string
  | Abs of string * typ * expr
  | App of expr * expr

type value =
  | VInt of int
  | VClosure of string * expr * value Context.t
  | VNative of (value -> value)

let rec eval context expr =
  let apply context l_expr r_expr =
    let arg = eval context r_expr in
      match eval context l_expr with
      | VClosure (param, body, context) ->
          let context = Context.add param arg context in
          eval context body 
      | VNative value -> value arg 
      | VInt _ -> assert false in

  match expr with
  | Var name -> (match Context.mem name context with
    | true -> Context.find name context
    | false -> failwith ("unbound variable: " ^ name))
  | Abs (param, _typ, body) -> VClosure (param, body, context)
  | Int number -> VInt number
  | App (l_expr, r_expr) -> apply context l_expr r_expr

let rec to_string expr =
  match expr with
  | Var name -> Printf.sprintf "Var %s" name
  | Abs (arg, _typ, body) -> Printf.sprintf "Abs(%s, %s)" arg (to_string body)
  | App (l_expr, r_expr) -> Printf.sprintf "App(%s, %s)" (to_string l_expr) (to_string r_expr)
  | Int number -> Printf.sprintf "Int %d" number

