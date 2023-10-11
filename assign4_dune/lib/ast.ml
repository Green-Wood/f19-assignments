open Core

type variable = string [@@deriving sexp, compare]

let fresh s = s ^ "'"

module Type = struct
  type t =
    | Num
    | Bool
    | Unit
    | Var of variable
    | Fn of
        { arg : t
        ; ret : t
        }
    | Product of
        { left : t
        ; right : t
        }
    | Sum of
        { left : t
        ; right : t
        }
    | Rec of
        { a : variable
        ; tau : t
        }
    | Forall of
        { a : variable
        ; tau : t
        }
    | Exists of
        { a : variable
        ; tau : t
        }
  [@@deriving variants, sexp, compare]

  let rec to_string ty =
    match ty with
    | Num -> "num"
    | Bool -> "bool"
    | Unit -> "unit"
    | Var x -> x
    | Fn { arg; ret } -> Printf.sprintf "(%s -> %s)" (to_string arg) (to_string ret)
    | Product { left; right } ->
      Printf.sprintf "(%s * %s)" (to_string left) (to_string right)
    | Sum { left; right } -> Printf.sprintf "(%s + %s)" (to_string left) (to_string right)
    | Rec { a; tau } -> Printf.sprintf "μ %s . %s" a (to_string tau)
    | Forall { a; tau } -> Printf.sprintf "∀ %s . %s" a (to_string tau)
    | Exists { a; tau } -> Printf.sprintf "∃ %s . %s" a (to_string tau)
  ;;

  let to_string_sexp ty = Sexp.to_string_hum (sexp_of_t ty)

  let rec substitute_map (rename : t String.Map.t) (tau : t) : t =
    match tau with
    | Num -> Num
    | Bool -> Bool
    | Fn { arg; ret } ->
      Fn { arg = substitute_map rename arg; ret = substitute_map rename ret }
    | Unit -> Unit
    | Product { left; right } ->
      Product { left = substitute_map rename left; right = substitute_map rename right }
    | Sum { left; right } ->
      Sum { left = substitute_map rename left; right = substitute_map rename right }
    (* Add more cases here! *)
    | _ -> Error.raise_s [%message "Type substitution unimplemented for" (tau : t)]
  ;;

  let substitute (x : string) ~(tau' : t) ~(tau : t) : t =
    substitute_map (String.Map.singleton x tau') tau
  ;;

  let rec to_debruijn (tau : t) : t =
    let rec aux (depth : int String.Map.t) (tau : t) : t =
      match tau with
      | Num -> Num
      | Bool -> Bool
      | Fn { arg; ret } -> Fn { arg = aux depth arg; ret = aux depth ret }
      | Unit -> Unit
      | Product { left; right } ->
        Product { left = aux depth left; right = aux depth right }
      | Sum { left; right } -> Sum { left = aux depth left; right = aux depth right }
      (* Add more cases here! *)
      | _ -> Error.raise_s [%message "Type debruijn unimplemented for" (tau : t)]
    in
    aux String.Map.empty tau
  ;;

  let compare tau1 tau2 = compare (to_debruijn tau1) (to_debruijn tau2)
  let equal = [%compare.equal: t]
end

module Expr = struct
  type binop =
    | Add
    | Sub
    | Mul
    | Div
  [@@deriving variants, sexp, compare, quickcheck]

  type relop =
    | Lt
    | Gt
    | Eq
  [@@deriving variants, sexp, compare]

  type direction =
    | Left
    | Right
  [@@deriving variants, sexp, compare]

  type t =
    | Num of int
    | Binop of
        { binop : binop
        ; left : t
        ; right : t
        }
    | True
    | False
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        }
    | Relop of
        { relop : relop
        ; left : t
        ; right : t
        }
    | And of
        { left : t
        ; right : t
        }
    | Or of
        { left : t
        ; right : t
        }
    | Var of variable
    | Lam of
        { x : variable (* TODO: May need to ignore for comparison *)
        ; tau : Type.t
        ; e : t
        }
    | App of
        { lam : t
        ; arg : t
        }
    | Unit
    | Pair of
        { left : t
        ; right : t
        }
    | Project of
        { e : t
        ; d : direction
        }
    | Inject of
        { e : t
        ; d : direction
        ; tau : Type.t
        }
    | Case of
        { e : t
        ; xleft : variable
        ; eleft : t
        ; xright : variable
        ; eright : t
        }
    | Fix of
        { x : variable
        ; tau : Type.t
        ; e : t
        }
    | TyLam of
        { a : variable
        ; e : t
        }
    | TyApp of
        { e : t
        ; tau : Type.t
        }
    | Fold_ of
        { e : t
        ; tau : Type.t
        }
    | Unfold of t
    | Export of
        { e : t
        ; tau_adt : Type.t
        ; tau_mod : Type.t
        }
    | Import of
        { x : variable
        ; a : variable
        ; e_mod : t
        ; e_body : t
        }
  [@@deriving variants, sexp, compare]

  let rec to_string e =
    match e with
    | Num n -> string_of_int n
    | Binop { binop; left; right } ->
      let bstr =
        match binop with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
      in
      Printf.sprintf "(%s %s %s)" (to_string left) bstr (to_string right)
    | Relop { relop; left; right } ->
      let rstr =
        match relop with
        | Eq -> "="
        | Lt -> "<"
        | Gt -> ">"
      in
      Printf.sprintf "(%s %s %s)" (to_string left) rstr (to_string right)
    | True -> "true"
    | False -> "false"
    | If { cond; then_; else_ } ->
      Printf.sprintf
        "(if %s then %s else %s)"
        (to_string cond)
        (to_string then_)
        (to_string else_)
    | And { left; right } ->
      Printf.sprintf "(%s && %s)" (to_string left) (to_string right)
    | Or { left; right } -> Printf.sprintf "(%s || %s)" (to_string left) (to_string right)
    | Var x -> x
    | Lam { x; tau; e } ->
      Printf.sprintf "(fun (%s : %s) -> %s)" x (Type.to_string tau) (to_string e)
    | App { lam; arg } -> Printf.sprintf "(%s %s)" (to_string lam) (to_string arg)
    | Unit -> "()"
    | Pair { left; right } -> Printf.sprintf "(%s, %s)" (to_string left) (to_string right)
    | Project { e; d } ->
      let dstr =
        match d with
        | Left -> "L"
        | Right -> "R"
      in
      Printf.sprintf "%s.%s" (to_string e) dstr
    | Inject { e; d; tau } ->
      let dstr =
        match d with
        | Left -> "L"
        | Right -> "R"
      in
      Printf.sprintf "(inj %s = %s as %s)" (to_string e) dstr (Type.to_string tau)
    | Case { e; xleft; eleft; xright; eright } ->
      Printf.sprintf
        "(case %s {L(%s) -> %s | R(%s) -> %s)"
        (to_string e)
        xleft
        (to_string eleft)
        xright
        (to_string eright)
    | Fix { x; tau; e } ->
      Printf.sprintf "(fix (%s : %s) -> %s)" x (Type.to_string tau) (to_string e)
    | TyLam { a; e } -> Printf.sprintf "(Λ %s -> %s)" a (to_string e)
    | TyApp { e; tau } -> Printf.sprintf "(%s [%s])" (to_string e) (Type.to_string tau)
    | Fold_ { e; tau } ->
      Printf.sprintf "(fold %s as %s)" (to_string e) (Type.to_string tau)
    | Unfold e -> Printf.sprintf "(unfold %s)" (to_string e)
    | Export { e; tau_adt; tau_mod } ->
      Printf.sprintf
        "(export %s without %s as %s)"
        (to_string e)
        (Type.to_string tau_adt)
        (Type.to_string tau_mod)
    | Import { x; a; e_mod; e_body } ->
      Printf.sprintf
        "(import (%s, %s) = %s in %s)"
        x
        a
        (to_string e_mod)
        (to_string e_body)
  ;;

  let to_string_sexp e = Sexp.to_string_hum (sexp_of_t e)

  let rec substitute_map (rename : t String.Map.t) (e : t) : t =
    match e with
    | Num _ -> e
    | Binop { binop; left; right } ->
      Binop
        { binop; left = substitute_map rename left; right = substitute_map rename right }
    | True -> True
    | False -> False
    | Relop { relop; left; right } ->
      Relop
        { relop; left = substitute_map rename left; right = substitute_map rename right }
    | And { left; right } ->
      And { left = substitute_map rename left; right = substitute_map rename right }
    | Or { left; right } ->
      Or { left = substitute_map rename left; right = substitute_map rename right }
    | If { cond; then_; else_ } ->
      If
        { cond = substitute_map rename cond
        ; then_ = substitute_map rename then_
        ; else_ = substitute_map rename else_
        }
    | Var x -> Map.find rename x |> Option.value ~default:(Var x)
    | Lam { x; tau; e } ->
      let fresh_x = fresh x in
      let rename = Map.set rename ~key:x ~data:(Var fresh_x) in
      Lam { x = fresh_x; tau; e = substitute_map rename e }
    | App { lam; arg } ->
      App { lam = substitute_map rename lam; arg = substitute_map rename arg }
    | Unit -> Unit
    | Pair { left; right } ->
      Pair { left = substitute_map rename left; right = substitute_map rename right }
    | Project { e; d } -> Project { e = substitute_map rename e; d }
    | Inject { e; d; tau } -> Inject { e = substitute_map rename e; d; tau }
    | Case { e; xleft; eleft; xright; eright } ->
      let fresh_xleft = fresh xleft in
      let fresh_xright = fresh xright in
      let rename_left = Map.set rename ~key:xleft ~data:(Var fresh_xleft) in
      let rename_right = Map.set rename ~key:xright ~data:(Var fresh_xright) in
      Case
        { e = substitute_map rename e
        ; xleft = fresh_xleft
        ; eleft = substitute_map rename_left eleft
        ; xright = fresh_xright
        ; eright = substitute_map rename_right eright
        }
    (* Put more cases here! *)
    | _ -> Error.raise_s [%message "Expr substitution unimplemented for" (e : t)]
  ;;

  let substitute (x : string) ~(e' : t) ~(e : t) : t =
    substitute_map (String.Map.singleton x e') e
  ;;

  let rec to_debruijn (e : t) : t =
    let rec aux (depth : int String.Map.t) (e : t) : t =
      match e with
      | Num _ -> e
      | Binop { binop; left; right } ->
        Binop { binop; left = aux depth left; right = aux depth right }
      | True -> True
      | False -> False
      | Relop { relop; left; right } ->
        Relop { relop; left = aux depth left; right = aux depth right }
      | And { left; right } -> And { left = aux depth left; right = aux depth right }
      | Or { left; right } -> Or { left = aux depth left; right = aux depth right }
      | If { cond; then_; else_ } ->
        If { cond = aux depth cond; then_ = aux depth then_; else_ = aux depth else_ }
      | Var x -> Map.find depth x |> Option.value_map ~default:x ~f:Int.to_string |> Var
      | Lam { x; tau; e } ->
        let depth = Map.map depth ~f:(( + ) 1) |> Map.set ~key:x ~data:0 in
        Lam { x = "_"; tau; e = aux depth e }
      | App { lam; arg } -> App { lam = aux depth lam; arg = aux depth arg }
      | Unit -> Unit
      | Pair { left; right } -> Pair { left = aux depth left; right = aux depth right }
      | Project { e; d } -> Project { e = aux depth e; d }
      | Inject { e; d; tau } -> Inject { e = aux depth e; d; tau }
      | Case { e; xleft; eleft; xright; eright } ->
        let depth_plus = Map.map depth ~f:(( + ) 1) in
        let depth_left = Map.set depth_plus ~key:xleft ~data:0 in
        let depth_right = Map.set depth_plus ~key:xright ~data:0 in
        Case
          { e = aux depth e
          ; xleft = "_"
          ; eleft = aux depth_left eleft
          ; xright = "_"
          ; eright = aux depth_right eright
          }
      (* Add more cases here! *)
      | _ -> Error.raise_s [%message "Expr debruijn unimplemented for" (e : t)]
    in
    aux String.Map.empty e
  ;;

  let compare e1 e2 = compare (to_debruijn e1) (to_debruijn e2)
  let equal = [%compare.equal: t]
end
