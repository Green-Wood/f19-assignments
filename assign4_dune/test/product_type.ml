open Core
open Lam

let%expect_test "unit" =
  Test_util.check_type_eval "()" Ast.Type.Unit Ast.Expr.Unit;
  Test_util.check_type_eval "(fun (x : unit) -> 0) ()" Ast.Type.Num (Ast.Expr.Num 0)
;;

let%expect_test "type" =
  let p_ex = Parser.parse_expr_exn in
  let p_ty = Parser.parse_type_exn in
  Test_util.check_type (p_ex "(1, (false, 0))") (p_ty "num * (bool * num)");
  Test_util.check_type (p_ex "(1, fun (x : num) -> x + 1)") (p_ty "num * (num -> num)");
  Test_util.check_type (p_ex "(1, (false, 0)).R.L") (p_ty "bool");
  Test_util.check_type (p_ex "(1, false).L + 0") (p_ty "num");
  Test_util.error_type (p_ex "(1, false).R + 0")
;;

let%expect_test "product" =
  Test_util.check_type_eval "(1, (false, true)).R.L || false" Ast.Type.Bool Ast.Expr.False;
  Test_util.check_type_eval
    "(1, fun (x : num) -> x + 1).R 10"
    Ast.Type.Num
    (Ast.Expr.Num 11);
  Test_util.check_type_eval
    {|
    let f : num -> num = fun (n : num) -> n + 1 in
    let t : num * bool = (1, true) in
    let n : num = (f (t.L)) in
    n
    |}
    Ast.Type.Num
    (Ast.Expr.Num 2)
;;
