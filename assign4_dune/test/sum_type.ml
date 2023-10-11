open Core
open Lam

let%expect_test "type" =
  let p_ex = Parser.parse_expr_exn in
  let p_ty = Parser.parse_type_exn in
  Test_util.check_type (p_ex "inj (1 + 2) = L as (num + bool)") (p_ty "num + bool");
  Test_util.check_type
    (p_ex "inj (fun (x : num) -> x + 1) = R as (bool + (num -> num))")
    (p_ty "bool + (num -> num)");
  Test_util.check_type
    (p_ex "case (inj (1 + 2) = L as (num + bool)) {L(x) -> x < 1 | R(b) -> b && true}")
    (p_ty "bool");
  Test_util.error_type (p_ex "inj (1 + 2) = R as (num + bool)");
  Test_util.error_type
    (p_ex "case (inj 0 = L as (num + bool)) {L(x) -> x + 1 | R(b) -> b && false}")
;;

let%expect_test "product" =
  Test_util.check_type_eval
    {|
    let s : num + bool = inj false = R as num + bool in
    let n : num = case s { L(s) -> s + 1 | R(s) -> if s then 1 else 0 } in
    n
    |}
    Ast.Type.Num
    (Ast.Expr.Num 0);
  Test_util.check_type_eval
    {|
    let x : num = 1 in
    let y : num + bool = inj 5 = L as num + bool in
    case y {
      L(z) -> z + x
    | R(z) -> 9
    }
    |}
    Ast.Type.Num
    (Ast.Expr.Num 6);
  Test_util.check_type_eval
    {|
    let x : num = 2 in
    let y : num + (num -> num) = inj (fun (x : num) -> x * x) = R as num + (num -> num) in
    case y {
      L(x) -> x
    | R(f) -> (f x)
    }
    |}
    Ast.Type.Num
    (Ast.Expr.Num 4);
  Test_util.check_type_eval
    {|
    let x : num = 2 in
    let y : num + (num -> num) = inj 1 = L as num + (num -> num) in
    case y {
      L(x) -> x
    | R(f) -> (f x)
    }
    |}
    Ast.Type.Num
    (Ast.Expr.Num 1)
;;
