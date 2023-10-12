open Core
open Lam

let%expect_test "type" =
  let p_ex = Parser.parse_expr_exn in
  let p_ty = Parser.parse_type_exn in
  Test_util.check_type (p_ex "tyfun a -> fun (x : a) -> x") (p_ty "forall a . (a -> a)");
  Test_util.check_type (p_ex "tyfun a -> fun (x : a) -> x") (p_ty "forall b . (b -> b)");
  Test_util.check_type
    (p_ex "let id : forall a . a -> a = tyfun b -> fun (x : b) -> x in (id [bool])")
    (p_ty "bool -> bool");
  Test_util.check_type
    (p_ex "let id : forall a . a -> a = tyfun b -> fun (x : b) -> x in (id [num])")
    (p_ty "num -> num");
  assert (
    not
      (Ast.Type.equal
         (p_ty "forall a . (forall b . a)")
         (p_ty "forall a . (forall b . b)")))
;;

let%expect_test "eval" =
  Test_util.check_type_eval
    {|
    let id : forall a . a -> a = tyfun a -> fun (x : a) -> x in
    let id_num : num -> num = (id [num]) in
    (id_num (1 + 2))
    |}
    Ast.Type.Num
    (Ast.Expr.Num 3);
  Test_util.check_type_eval
    {|
    let rev : forall a . forall b . (a * b) -> (b * a) =
      tyfun a -> tyfun b -> fun (p : (a * b)) -> (p.R, p.L) 
    in
    let i : (num * num) = (1, 2) in
    let o : (num * num) = ((rev [num] [num]) i) in
    (o.L = 2) && (o.R = 1)
    |}
    Ast.Type.Bool
    Ast.Expr.True;
  Test_util.check_type_eval
    {|
    let none : forall a . unit + a = tyfun a -> (inj () = L as unit + a) in
    let some : forall a . a -> (unit + a) =
      tyfun a -> fun (x : a) -> (inj x = R as unit + a)
    in
    let option_map : forall a . forall b . (a -> b) -> (unit + a) -> (unit + b) = 
      tyfun a -> tyfun b -> fun (f : (a -> b)) -> fun (i : (unit + a)) ->
      case i {
        L(x) -> none [b]
      | R(x) -> (some [b] (f x))
      }
    in
    let a : unit + num = (some [num] 1) in
    let b : unit + bool = (option_map [num] [bool] (fun (x : num) -> x = 1) a) in 
    case b {
      L(x) -> false
    | R(n) -> true
    }
    |}
    Ast.Type.Bool
    Ast.Expr.True
;;
