module Context : module type of Map.Make(String)

type typ =
  | TAbs of typ * typ
  | TInt

val eq : typ -> typ -> bool

val to_string : typ -> string

val infer : typ Context.t -> Ast.expr -> typ
