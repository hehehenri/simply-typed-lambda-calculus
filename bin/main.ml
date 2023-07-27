open Lambda_calculus

let input = "(\\x.x x) (\\x.x x)"
let tokens = Lexer.lex input
let ast = Parser.parse tokens
let _result = Ast.eval Ast.Context.empty ast

