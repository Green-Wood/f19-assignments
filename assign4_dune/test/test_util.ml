open Core
open Lam

let check_expr raw_expr ty target =
  let expr = Parser.parse_expr_exn raw_expr in
  [%test_result: (Ast.Type.t, string) Result.t] ~expect:(Ok ty) (Typecheck.typecheck expr);
  [%test_result: (Ast.Expr.t, string) Result.t]
    ~expect:(Ok target)
    (Interpreter.eval expr)
;;
