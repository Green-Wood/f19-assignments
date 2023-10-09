open Core
open Lam

let%expect_test "rel_op" =
  Test_util.check_expr "1 < 2" Ast.Type.Bool Ast.Expr.True;
  Test_util.check_expr "1 < 0" Ast.Type.Bool Ast.Expr.False;
  Test_util.check_expr "1 > 2" Ast.Type.Bool Ast.Expr.False;
  Test_util.check_expr "1 > 0" Ast.Type.Bool Ast.Expr.True;
  Test_util.check_expr "1 = 2" Ast.Type.Bool Ast.Expr.False;
  Test_util.check_expr "1 = 1" Ast.Type.Bool Ast.Expr.True;
  Test_util.check_expr "(1 + 2) > (3 - 1)" Ast.Type.Bool Ast.Expr.True
;;
