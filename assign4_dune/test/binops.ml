open Core
open Lam

let%expect_test "test_add" =
  Test_util.check_type_eval "(1 + 2) / 3 * 4 - 2" Ast.Type.Num (Ast.Expr.Num 2)
;;

module Binops_expr = struct
  type t =
    | Value of int
    | Op of t * Ast.Expr.binop * t
  [@@deriving quickcheck, sexp]

  let rec eval = function
    | Value n -> n
    | Op (left, op, right) ->
      let n1 = eval left in
      let n2 = eval right in
      let f =
        match op with
        | Ast.Expr.Add -> ( + )
        | Ast.Expr.Sub -> ( - )
        | Ast.Expr.Mul -> ( * )
        | Ast.Expr.Div -> ( / )
      in
      f n1 n2
  ;;

  let rec to_string = function
    | Value n -> Int.to_string n
    | Op (left, op, right) ->
      let s1 = to_string left in
      let s2 = to_string right in
      let op =
        match op with
        | Ast.Expr.Add -> "+"
        | Ast.Expr.Sub -> "-"
        | Ast.Expr.Mul -> "*"
        | Ast.Expr.Div -> "/"
      in
      [%string "(%{s1}) %{op} (%{s2})"]
  ;;
end

let%expect_test "quickcheck" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: Binops_expr.t]
    [%quickcheck.generator: Binops_expr.t]
    ~f:(fun t ->
      let str = Binops_expr.to_string t in
      try
        let target = Binops_expr.eval t |> Ast.Expr.Num in
        Test_util.check_type_eval str Ast.Type.Num target
      with
      | Division_by_zero -> ())
;;
