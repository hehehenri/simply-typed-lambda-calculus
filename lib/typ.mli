open Ast

val eq : typ -> typ -> bool

val to_string : typ -> string

val infer : typ Context.t -> expr -> typ
