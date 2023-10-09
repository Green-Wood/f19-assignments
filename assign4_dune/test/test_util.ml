open Core
open Lam

let check_expr raw_expr ty target =
  let expr = Parser.parse_expr_exn raw_expr in
  [%test_eq: (Ast.Type.t, string) Result.t] (Typecheck.typecheck expr) (Ok ty);
  [%test_eq: (Ast.Expr.t, string) Result.t] (Interpreter.eval expr) (Ok target)
;;
