open Core
open Ast

let rec typecheck_expr (ctx : Type.t String.Map.t) (e : Expr.t)
  : (Type.t, string) Result.t
  =
  match e with
  | Expr.Num _ -> Ok Type.Num
  | Expr.Binop { left; right; _ } ->
    let%bind.Result tau_left = typecheck_expr ctx left in
    let%bind.Result tau_right = typecheck_expr ctx right in
    (match tau_left, tau_right with
     | Type.Num, Type.Num -> Ok Type.Num
     | _ ->
       Error
         [%string
           "Binary operands have incompatible types: (%{left#Expr} : %{tau_left#Type}) \
            and (%{right#Expr} : %{tau_right#Type})"])
  | Expr.Relop { left; right; _ } ->
    let%bind.Result tau_left = typecheck_expr ctx left in
    let%bind.Result tau_right = typecheck_expr ctx right in
    (match tau_left, tau_right with
     | Type.Num, Type.Num -> Ok Type.Bool
     | _ ->
       Error
         [%string
           "Relation operands have incompatible types: (%{left#Expr} : %{tau_left#Type}) \
            and (%{right#Expr} : %{tau_right#Type})"])
  | Expr.True -> Ok Type.Bool
  | Expr.False -> Ok Type.Bool
  | Expr.And { left; right } ->
    let%bind.Result tau_left = typecheck_expr ctx left in
    let%bind.Result tau_right = typecheck_expr ctx right in
    (match tau_left, tau_right with
     | Type.Bool, Type.Bool -> Ok Type.Bool
     | _ ->
       Error
         [%string
           "AND operands have incompatible types: (%{left#Expr} : %{tau_left#Type}) and \
            (%{right#Expr} : %{tau_right#Type})"])
  | Expr.Or { left; right } ->
    let%bind.Result tau_left = typecheck_expr ctx left in
    let%bind.Result tau_right = typecheck_expr ctx right in
    (match tau_left, tau_right with
     | Type.Bool, Type.Bool -> Ok Type.Bool
     | _ ->
       Error
         [%string
           "OR operands have incompatible types: (%{left#Expr} : %{tau_left#Type}) and \
            (%{right#Expr} : %{tau_right#Type})"])
  (* Add more cases here! *)
  | _ -> failwith [%string "Unimplemented typecheck for expr: %{e#Expr}"]
;;

let typecheck t = typecheck_expr String.Map.empty t

let inline_tests () =
  let p_ex = Parser.parse_expr_exn in
  let p_ty = Parser.parse_type_exn in
  let e1 = p_ex "fun (x : num) -> x" in
  let equal_type = [%compare.equal: (Type.t, variable) Result.t] in
  assert (equal_type (typecheck e1) (Ok (p_ty "num -> num")));
  let e2 = p_ex "fun (x : num) -> y" in
  assert (Result.is_error (typecheck e2));
  let t3 = p_ex "(fun (x : num) -> x) 3" in
  assert (equal_type (typecheck t3) (Ok (p_ty "num")));
  let t4 = p_ex "((fun (x : num) -> x) 3) 3" in
  assert (Result.is_error (typecheck t4));
  let t5 = p_ex "0 + (fun (x : num) -> x)" in
  assert (Result.is_error (typecheck t5))
;;

(* Uncomment the line below when you want to run the inline tests. *)
(* TODO move to test *)
(* let () = inline_tests () *)
