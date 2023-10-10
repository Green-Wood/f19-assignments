open Core
open Lam

let check_type expr ty =
  [%test_result: (Ast.Type.t, string) Result.t] ~expect:(Ok ty) (Typecheck.typecheck expr)
;;

let error_type expr =
  [%test_pred: (Ast.Type.t, string) Result.t] Result.is_error (Typecheck.typecheck expr)
;;

let check_type_eval raw_expr ty target =
  let expr = Parser.parse_expr_exn raw_expr in
  check_type expr ty;
  [%test_result: (Ast.Expr.t, string) Result.t]
    ~expect:(Ok target)
    (Interpreter.eval expr)
;;
