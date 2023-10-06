open Core
open Lam

let%expect_test "test_add" =
  let expr = Parser.parse_expr_exn "1 + 2" in
  print_s [%message (Typecheck.typecheck expr : (Ast.Type.t, string) Result.t)];
  [%expect {| ("Typecheck.typecheck expr" (Ok Num)) |}];
  print_s [%message (Interpreter.eval expr : (Ast.Expr.t, string) Result.t)];
  [%expect {| ("Interpreter.eval expr" (Ok (Num 3))) |}]
;;

(* TODO quickcheck *)
