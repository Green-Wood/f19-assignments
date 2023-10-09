open Core
open Lam

let check_expr raw_expr ty target =
  let expr = Parser.parse_expr_exn raw_expr in
  [%test_result: (Ast.Type.t, string) Result.t]
    ~equal:(Result.equal Ast.Type.aequiv String.equal)
    ~expect:(Ok ty)
    (Typecheck.typecheck expr);
  [%test_result: (Ast.Expr.t, string) Result.t]
    ~equal:(Result.equal Ast.Expr.aequiv String.equal)
    ~expect:(Ok target)
    (Interpreter.eval expr)
;;
