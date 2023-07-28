open Lambda_calculus
open Ast

let ast = Ast.(App(Var("print"), Int(5)))

let t_context =
  let context = Context.empty in
  let print = TAbs(TInt, TInt) in
  Context.add "print" print context

let v_context = 
  Ast.(
  let context = Context.empty in

  let print = VNative (fun (value:value) ->
    (match value with
    | VInt int -> print_int int
    | _ -> assert false);
    value
  ) in

  Context.add "print" print context)

let _infer = Typ.infer t_context ast

let _result = Ast.eval v_context ast
