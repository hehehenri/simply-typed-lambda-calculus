open Lambda_calculus
open Ast

let t_context =
  let context = Context.empty in
  let print = () in
  Context.add "print" print context

let v_context = 
  let context = Context.empty in

  let print = VNative (fun (value:value) ->
    (match value with
    | VInt int -> print_int int
    | _ -> assert false);
    value
  ) in

  Context.add "print" print context

let ast = App(Var("print"), Int(5))

let infer = 

let _result = eval context ast
