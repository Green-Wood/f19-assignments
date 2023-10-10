open Core
open Lam

let%expect_test "rel_op" =
  let ty = Ast.Type.Bool in
  Test_util.check_type_eval "1 < 2" ty Ast.Expr.True;
  Test_util.check_type_eval "1 < 0" ty Ast.Expr.False;
  Test_util.check_type_eval "1 > 2" ty Ast.Expr.False;
  Test_util.check_type_eval "1 > 0" ty Ast.Expr.True;
  Test_util.check_type_eval "1 = 2" ty Ast.Expr.False;
  Test_util.check_type_eval "1 = 1" ty Ast.Expr.True;
  Test_util.check_type_eval "(1 + 2) > (3 - 1)" ty Ast.Expr.True
;;

let%expect_test "bool_op" =
  let ty = Ast.Type.Bool in
  Test_util.check_type_eval "true && true" ty Ast.Expr.True;
  Test_util.check_type_eval "true && false" ty Ast.Expr.False;
  Test_util.check_type_eval "false && true" ty Ast.Expr.False;
  Test_util.check_type_eval "false && false" ty Ast.Expr.False;
  Test_util.check_type_eval "true || true" ty Ast.Expr.True;
  Test_util.check_type_eval "true || false" ty Ast.Expr.True;
  Test_util.check_type_eval "false || true" ty Ast.Expr.True;
  Test_util.check_type_eval "false || false" ty Ast.Expr.False;
  Test_util.check_type_eval
    "(1 + 2) > (3 - 1) && (3 - 2) = (2 - 1) || 1 > 2"
    ty
    Ast.Expr.True
;;

let%expect_test "if_else" =
  Test_util.check_type_eval "if true then 1 else 0" Ast.Type.Num (Ast.Expr.Num 1);
  Test_util.check_type_eval "if false then 1 else 0" Ast.Type.Num (Ast.Expr.Num 0);
  Test_util.check_type_eval
    "if (1 < 2) && (2 < 3) then 1 + 2 else 1 - 2"
    Ast.Type.Num
    (Ast.Expr.Num 3);
  Test_util.check_type_eval
    "if (1 < 2) && (4 < 3) then 1 + 2 else 1 - 2"
    Ast.Type.Num
    (Ast.Expr.Num (-1));
  Test_util.check_type_eval
    {|
    if (1 > 2) && (2 < 3) then 0
    else if 1 < 0 then 1
    else 2
    |}
    Ast.Type.Num
    (Ast.Expr.Num 2)
;;
