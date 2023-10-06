open Core
open Lam

let%expect_test "test_add" =
  let expr = Parser.parse_expr_exn "1 + 2" in
  Typecheck.typecheck expr |> [%sexp_of: (Ast.Type.t, string) Result.t] |> print_s;
  [%expect {| (Ok Num) |}];
  Interpreter.eval expr |> [%sexp_of: (Ast.Expr.t, string) Result.t] |> print_s;
  [%expect {| (Ok (Num 3)) |}]
;;

(* TODO quickcheck *)
