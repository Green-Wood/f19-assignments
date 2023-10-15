open Core
open Ast

type outcome =
  | Step of Expr.t
  | Val

exception RuntimeError of string

let rec trystep (e : Expr.t) : outcome =
  match e with
  | Expr.Lam _
  | Expr.Num _
  | Expr.True
  | Expr.False
  | Expr.Pair _
  | Expr.Unit
  | Expr.Inject _
  | Expr.TyLam _
  | Expr.Export _
  | Expr.Fold_ _ -> Val
  | Expr.Binop { binop; left; right } ->
    (left, fun left' -> Expr.Binop { left = left'; binop; right })
    |-> fun () ->
    (right, fun right' -> Expr.Binop { right = right'; binop; left })
    |-> fun () ->
    let n1, n2 =
      match left, right with
      | Expr.Num n1, Expr.Num n2 -> n1, n2
      | _ -> failwith "The oprands for given binop is not type [num]"
    in
    let f =
      match binop with
      | Expr.Add -> ( + )
      | Expr.Sub -> ( - )
      | Expr.Mul -> ( * )
      | Expr.Div -> ( / )
    in
    Step (Expr.Num (f n1 n2))
  | Expr.Relop { relop; left; right } ->
    (left, fun left' -> Expr.Relop { left = left'; relop; right })
    |-> fun () ->
    (right, fun right' -> Expr.Relop { right = right'; relop; left })
    |-> fun () ->
    let n1, n2 =
      match left, right with
      | Expr.Num n1, Expr.Num n2 -> n1, n2
      | _ -> failwith "The oprands for given relop is not type [num]"
    in
    let f =
      match relop with
      | Expr.Lt -> ( < )
      | Expr.Gt -> ( > )
      | Expr.Eq -> ( = )
    in
    Step (if f n1 n2 then Expr.True else Expr.False)
  | Expr.And { left; right } ->
    (left, fun left' -> Expr.And { left = left'; right })
    |-> fun () ->
    (right, fun right' -> Expr.And { right = right'; left })
    |-> fun () ->
    let result =
      match left, right with
      | Expr.True, Expr.True -> Expr.True
      | Expr.False, Expr.True -> Expr.False
      | Expr.True, Expr.False -> Expr.False
      | Expr.False, Expr.False -> Expr.False
      | _ -> failwith "The oprands for AND is not type [Bool]"
    in
    Step result
  | Expr.Or { left; right } ->
    (left, fun left' -> Expr.Or { left = left'; right })
    |-> fun () ->
    (right, fun right' -> Expr.Or { right = right'; left })
    |-> fun () ->
    let result =
      match left, right with
      | Expr.True, Expr.True -> Expr.True
      | Expr.False, Expr.True -> Expr.True
      | Expr.True, Expr.False -> Expr.True
      | Expr.False, Expr.False -> Expr.False
      | _ -> failwith "The oprands for OR is not type [Bool]"
    in
    Step result
  | Expr.If { cond; then_; else_ } ->
    (cond, fun cond' -> Expr.If { cond = cond'; then_; else_ })
    |-> fun () ->
    (match cond with
     | Expr.True -> Step then_
     | Expr.False -> Step else_
     | _ -> failwith "The type of cond in if-else is not type [Bool]")
  | Expr.App { lam; arg } ->
    (lam, fun lam' -> Expr.App { lam = lam'; arg })
    |-> fun () ->
    (arg, fun arg' -> Expr.App { lam; arg = arg' })
    |-> fun () ->
    (match lam with
     | Expr.Lam { x; tau; e } -> Step (Expr.substitute x ~e':arg ~e)
     | _ -> failwith "The type of lam is not type [Fn]")
  | Expr.Project { e; d } ->
    (e, fun e' -> Expr.Project { e = e'; d })
    |-> fun () ->
    (match e with
     | Expr.Pair { left; right } ->
       (match d with
        | Expr.Left -> Step left
        | Expr.Right -> Step right)
     | _ -> failwith "Type project is not type [Pair]")
  | Expr.Case { e; xleft; eleft; xright; eright } ->
    (e, fun e' -> Expr.Case { e = e'; xleft; eleft; xright; eright })
    |-> fun () ->
    (match e with
     | Expr.Inject { e = e'; d; tau } ->
       (match d with
        | Expr.Left -> Expr.substitute xleft ~e' ~e:eleft
        | Expr.Right -> Expr.substitute xright ~e' ~e:eright)
       |> Step
     | _ -> failwith "Type of case statement is not type [Inject]")
  | Expr.Fix { x; tau; e } as e' -> Step (Expr.substitute x ~e' ~e)
  | Expr.TyApp { e; tau } ->
    (e, fun e' -> Expr.TyApp { e = e'; tau })
    |-> fun () ->
    (match e with
     | Expr.TyLam { a; e } -> Step e
     | _ -> failwith "Type of type-level application is not type [TyLam]")
  | Expr.Unfold e ->
    (e, fun e' -> Expr.Unfold e')
    |-> fun () ->
    (match e with
     | Expr.Fold_ { e; tau } -> Step e
     | _ -> failwith "Type of that apply unfold should be another fold")
  | Expr.Import { x; a; e_mod; e_body } ->
    (e_mod, fun e_mod' -> Expr.Import { x; a; e_mod = e_mod'; e_body })
    |-> fun () ->
    (match e_mod with
     | Expr.Export { e = e'; tau_adt; tau_mod } -> Step (Expr.substitute x ~e' ~e:e_body)
     | _ -> failwith "Type of import should be export")
  (* Add more cases here! *)
  | Expr.Var _ ->
    raise
      (RuntimeError
         (Printf.sprintf "Reached a stuck state at expression: %s" (Expr.to_string e)))

and ( |-> ) ((e, hole) : Expr.t * (Expr.t -> Expr.t)) (next : unit -> outcome) : outcome =
  match trystep e with
  | Step e' -> Step (hole e')
  | Val -> next ()
;;

let rec eval e =
  match trystep e with
  | Step e' ->
    if Flags.extra_verbose ()
    then Printf.printf "Stepped:\n%s\n|->\n%s\n\n" (Expr.to_string e) (Expr.to_string e');
    eval e'
  | Val -> Ok e
;;
