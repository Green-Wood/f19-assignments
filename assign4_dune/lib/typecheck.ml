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
  | Expr.If { cond; then_; else_ } ->
    let%bind.Result tau_cond = typecheck_expr ctx cond in
    let%bind.Result tau_then = typecheck_expr ctx then_ in
    let%bind.Result tau_else = typecheck_expr ctx else_ in
    (match tau_cond with
     | Type.Bool ->
       if Type.equal tau_then tau_else
       then Ok tau_then
       else
         Error
           [%string
             "The type of [then] and [else] is incompatible: (%{then_#Expr} : \
              %{tau_then#Type}) and (%{else_#Expr} : %{tau_else#Type})"]
     | _ ->
       Error
         [%string
           "The condition of if-else should be of type [Bool] but got: (%{cond#Expr} : \
            %{tau_cond#Type})"])
  | Expr.Var var ->
    Map.find ctx var
    |> Result.of_option
         ~error:
           [%string "The type of variable [%{var}] cannot be inferred from the context."]
  | Expr.Lam { x; tau; e } ->
    let%map.Result tau_ret = typecheck_expr (Map.set ctx ~key:x ~data:tau) e in
    Type.Fn { arg = tau; ret = tau_ret }
  | Expr.App { lam; arg } ->
    let%bind.Result tau_lam = typecheck_expr ctx lam in
    let%bind.Result tau_arg = typecheck_expr ctx arg in
    (match tau_lam with
     | Type.Fn { arg = arg'; ret } ->
       if Type.equal arg' tau_arg
       then Ok ret
       else
         Error
           [%string
             "The type of function and argument is incompatible:\n\
              (%{lam#Expr} : %{tau_lam#Type})\n\
              (%{arg#Expr} : %{tau_arg#Type})"]
     | _ ->
       Error
         [%string
           "The type of lambda should be a function, but got: (%{lam#Expr} : \
            (%{tau_lam#Type}))"])
  | Expr.Unit -> Ok Type.Unit
  | Expr.Pair { left; right } ->
    let%bind.Result tau_left = typecheck_expr ctx left in
    let%map.Result tau_right = typecheck_expr ctx right in
    Type.Product { left = tau_left; right = tau_right }
  | Expr.Project { e; d } ->
    let%bind.Result tau = typecheck_expr ctx e in
    (match tau with
     | Type.Product { left; right } ->
       (match d with
        | Expr.Left -> Ok left
        | Expr.Right -> Ok right)
     | _ ->
       Error
         [%string
           "The type of project should be a [Product], but got: (%{e#Expr} : %{tau#Type})"])
  | Expr.Inject { e; d; tau } ->
    let%bind.Result tau_e = typecheck_expr ctx e in
    (match tau with
     | Type.Sum { left; right } ->
       let tau_target =
         match d with
         | Expr.Left -> left
         | Expr.Right -> right
       in
       if Type.equal tau_e tau_target
       then Ok tau
       else
         Error
           [%string
             "The type of inject expression and [Sum] type is incompatible: (%{e#Expr} : \
              %{tau_e#Type}), ( : %{tau_target#Type})"]
     | _ ->
       Error
         [%string "The result type of inject should be a [Sum], but got: (%{tau#Type})"])
  | Expr.Case { e; xleft; eleft; xright; eright } ->
    let%bind.Result tau_e = typecheck_expr ctx e in
    (match tau_e with
     | Type.Sum { left; right } ->
       let%bind.Result tau_left =
         typecheck_expr (Map.set ctx ~key:xleft ~data:left) eleft
       in
       let%bind.Result tau_right =
         typecheck_expr (Map.set ctx ~key:xright ~data:right) eright
       in
       if Type.equal tau_left tau_right
       then Ok tau_left
       else
         Error
           [%string
             "The type in two case branches is incompatible: (%{eleft#Expr} : \
              %{tau_left#Type}), (%{eright#Expr} : %{tau_right#Type})"]
     | _ ->
       Error
         [%string
           "The type of expr in [case] should be a [Sum], but got: (%{tau_e#Type})"])
  | Expr.Fix { x; tau; e } ->
    let%bind.Result tau_ret = typecheck_expr (Map.set ctx ~key:x ~data:tau) e in
    if Type.equal tau tau_ret
    then Ok tau
    else
      Error
        [%string
          "The type of fix expr should be consistent, but got (x : %{tau#Type}), \
           (%{e#Expr} : %{tau_ret#Type})"]
  | Expr.TyLam { a; e } ->
    let%map.Result tau = typecheck_expr ctx e in
    Type.Forall { a; tau }
  | Expr.TyApp { e; tau = tau' } ->
    let%bind.Result tau_e = typecheck_expr ctx e in
    (match tau_e with
     | Type.Forall { a; tau } -> Ok (Type.substitute a ~tau' ~tau)
     | _ ->
       Error
         [%string
           "The expr type of type-level application should be [Forall], but got \
            (%{tau_e#Type})"])
  | Expr.Fold_ { e; tau } ->
    (match tau with
     | Type.Rec { a; tau } as tau' ->
       let%bind.Result tau_e = typecheck_expr ctx e in
       let target_tau_e = Type.substitute a ~tau' ~tau in
       if Type.equal target_tau_e tau_e
       then Ok tau'
       else
         Error
           [%string
             "The type of expr and rec type is incompatible: (%{e#Expr} : \
              %{tau_e#Type}), ( : %{target_tau_e#Type})"]
     | _ ->
       Error [%string "The expr that fold need to be of type [Rec], but got %{tau#Type}"])
  | Expr.Unfold e ->
    let%bind.Result tau_e = typecheck_expr ctx e in
    (match tau_e with
     | Type.Rec { a; tau } as tau' -> Ok (Type.substitute a ~tau' ~tau)
     | _ ->
       Error [%string "The type of unfold expr should be [Rec], but got %{tau_e#Type}"])
  | Expr.Export { e; tau_adt; tau_mod } ->
    (match tau_mod with
     | Type.Exists { a; tau } ->
       let%bind.Result tau_e = typecheck_expr ctx e in
       let target_tau_e = Type.substitute a ~tau':tau_adt ~tau in
       if Type.equal target_tau_e tau_e
       then Ok tau_mod
       else
         Error
           [%string
             "The type of expr and export [Exists] type is incompatible: (expr : \
              %{tau_e#Type}), (exists: target_tau_e##Type)"]
     | _ ->
       Error
         [%string "The type of export expr should be [Exists], but got %{tau_mod#Type}"])
  | Expr.Import { x; a = beta; e_mod; e_body } ->
    let%bind.Result tau_mod = typecheck_expr ctx e_mod in
    (match tau_mod with
     | Type.Exists { a; tau } ->
       let tau_x = Type.substitute a ~tau':(Var beta) ~tau in
       typecheck_expr (Map.set ctx ~key:x ~data:tau_x) e_body
     | _ ->
       Error [%string "The expr that import should be [Exists], but got %{tau_mod#Type}"])
;;

let typecheck t = typecheck_expr String.Map.empty t
