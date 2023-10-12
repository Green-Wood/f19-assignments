open Core
open Lam

let%expect_test "rec type" =
  let p_ex = Parser.parse_expr_exn in
  let p_ty = Parser.parse_type_exn in
  Test_util.check_type (p_ex "letrec x : num = x + 1 in x") (p_ty "num");
  Test_util.error_type (p_ex "letrec x : num = x > 1 in x")
;;

let%expect_test "alpha equiv" =
  let p = Parser.parse_expr_exn in
  [%test_eq: Ast.Expr.t]
    (p "letrec x : num = x + 1 in x")
    (p "letrec y : num = y + 1 in y")
;;

let%expect_test "fib" =
  Test_util.check_type_eval
    {|
    letrec fib : (num -> num) = fun (n : num) ->
      if n < 2 then n else (fib (n - 1)) + (fib (n - 2))
    in
    (fib 7)
    |}
    Ast.Type.Num
    (Ast.Expr.Num 13)
;;
