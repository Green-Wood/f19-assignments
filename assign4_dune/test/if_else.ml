open Core
open Lam

let%expect_test "rel_op" =
  let ty = Ast.Type.Bool in
  Test_util.check_expr "1 < 2" ty Ast.Expr.True;
  Test_util.check_expr "1 < 0" ty Ast.Expr.False;
  Test_util.check_expr "1 > 2" ty Ast.Expr.False;
  Test_util.check_expr "1 > 0" ty Ast.Expr.True;
  Test_util.check_expr "1 = 2" ty Ast.Expr.False;
  Test_util.check_expr "1 = 1" ty Ast.Expr.True;
  Test_util.check_expr "(1 + 2) > (3 - 1)" ty Ast.Expr.True
;;

let%expect_test "bool_op" =
  let ty = Ast.Type.Bool in
  Test_util.check_expr "true && true" ty Ast.Expr.True;
  Test_util.check_expr "true && false" ty Ast.Expr.False;
  Test_util.check_expr "false && true" ty Ast.Expr.False;
  Test_util.check_expr "false && false" ty Ast.Expr.False;
  Test_util.check_expr "true || true" ty Ast.Expr.True;
  Test_util.check_expr "true || false" ty Ast.Expr.True;
  Test_util.check_expr "false || true" ty Ast.Expr.True;
  Test_util.check_expr "false || false" ty Ast.Expr.False;
  Test_util.check_expr "(1 + 2) > (3 - 1) && (3 - 2) = (2 - 1) || 1 > 2" ty Ast.Expr.True
;;
