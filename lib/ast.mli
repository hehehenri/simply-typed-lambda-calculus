module Context : module type of Map.Make(String)

type expr =
  | Int of int
  | Var of string
  | Abs of string * expr
  | App of expr * expr

type value =
  | VInt of int
  | VClosure of string * expr * value Context.t
  | VNative of (value -> value)

val eval : value Context.t -> expr -> value

val to_string : expr -> string
