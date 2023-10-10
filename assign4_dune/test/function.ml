open Core
open Lam

let%expect_test "lamda" =
  let t = Ast.Type.Fn { arg = Ast.Type.Num; ret = Ast.Type.Num } in
  Test_util.check_type_eval
    "fun (n : num) -> n + 1"
    t
    (Ast.Expr.Lam
       { x = "n"
       ; tau = Ast.Type.Num
       ; e =
           Ast.Expr.Binop
             { binop = Ast.Expr.Add; left = Ast.Expr.Var "n"; right = Ast.Expr.Num 1 }
       })
;;

let%expect_test "aequiv expr" =
  let e1 = Parser.parse_expr_exn "fun (n: num) -> n + 1" in
  let e2 = Parser.parse_expr_exn "fun (x: num) -> x + 1" in
  [%test_eq: Ast.Expr.t] e1 e2;
  let p = Parser.parse_expr_exn in
  assert (
    not
      (Ast.Expr.equal
         (p "fun (x : num) -> fun (x : num) -> x + x")
         (p "fun (x : num) -> fun (y : num) -> y + x")))
;;

let%expect_test "substitution expr" =
  let p = Parser.parse_expr_exn in
  let t1 = p "(fun (x : num) -> x) y" in
  [%test_eq: Ast.Expr.t] (Ast.Expr.substitute "x" ~e':(Num 0) ~e:t1) t1;
  [%test_eq: Ast.Expr.t]
    (Ast.Expr.substitute "y" ~e':(Num 0) ~e:t1)
    (p "(fun (x : num) -> x) 0");
  let t2 = p "x + (fun (x : num) -> y)" in
  [%test_eq: Ast.Expr.t]
    (Ast.Expr.substitute "x" ~e':(Num 0) ~e:t2)
    (p "0 + (fun (x : num) -> y)");
  [%test_eq: Ast.Expr.t]
    (Ast.Expr.substitute "y" ~e':(Num 0) ~e:t2)
    (p "x + (fun (x : num) -> 0)")
;;

let%expect_test "function typecheck" =
  let p_ex = Parser.parse_expr_exn in
  let p_ty = Parser.parse_type_exn in
  let e1 = p_ex "fun (x : num) -> x" in
  Test_util.check_type e1 (p_ty "num -> num");
  let e2 = p_ex "fun (x : num) -> y" in
  Test_util.error_type e2;
  let t3 = p_ex "(fun (x : num) -> x) 3" in
  Test_util.check_type t3 (p_ty "num");
  let t4 = p_ex "((fun (x : num) -> x) 3) 3" in
  Test_util.error_type t4;
  let t5 = p_ex "0 + (fun (x : num) -> x)" in
  Test_util.error_type t5
;;

let%expect_test "function eval" =
  Test_util.check_type_eval
    "(fun (x : num) -> fun (x : num) -> (x + x)) 1 2"
    Ast.Type.Num
    (Ast.Expr.Num 4);
  Test_util.check_type_eval
    {|
    let f : num -> num -> num = fun (x : num) -> fun (x : num) -> x in
    (f 0) 1
    |}
    Ast.Type.Num
    (Ast.Expr.Num 1);
  Test_util.check_type_eval
    {|
    let n : num = 1 + 2 in
    let n : num = n + 1 in
    let n : num = -100 in
    n - 1
    |}
    Ast.Type.Num
    (Ast.Expr.Num (-101));
  Test_util.check_type_eval
    {|
    let n : num = 0 in
    let f1 : num -> num = fun (n : num) -> n + 1 in
    let f2 : num -> num = fun (n : num) -> n + 2 in
    let n : bool = (f2 n) > (f1 n) in
    n
    |}
    Ast.Type.Bool
    Ast.Expr.True;
  Test_util.check_type_eval
    {|
    let n : num = 0 in
    let x : num = 
      let n : num = n + 1 in
      n * 3
    in
    n
    |}
    Ast.Type.Num
    (Ast.Expr.Num 0)
;;
