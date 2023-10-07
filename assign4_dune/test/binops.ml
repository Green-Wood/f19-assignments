open Core
open Lam

let%expect_test "test_add" =
  let expr = Parser.parse_expr_exn "(1 + 2) / 3 * 4 - 2" in
  print_s [%message (Typecheck.typecheck expr : (Ast.Type.t, string) Result.t)];
  [%expect {| ("Typecheck.typecheck expr" (Ok Num)) |}];
  print_s [%message (Interpreter.eval expr : (Ast.Expr.t, string) Result.t)];
  [%expect {| ("Interpreter.eval expr" (Ok (Num 2))) |}]
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
        let target = Binops_expr.eval t |> Ast.Expr.Num |> Ok in
        let expr = Parser.parse_expr_exn str in
        [%test_eq: (Ast.Type.t, string) Result.t]
          (Typecheck.typecheck expr)
          (Ok Ast.Type.Num);
        [%test_eq: (Ast.Expr.t, string) Result.t] (Interpreter.eval expr) target
      with
      | Division_by_zero -> ())
;;
