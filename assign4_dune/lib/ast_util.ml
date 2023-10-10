open Core

module Type = struct
  open Ast.Type

  let rec aequiv (tau1 : t) (tau2 : t) : bool =
    let rec aux (tau1 : t) (tau2 : t) : bool =
      match tau1, tau2 with
      | Num, Num -> true
      | Bool, Bool | Unit, Unit -> true
      | Var x, Var y -> String.equal x y
      | Fn x, Fn y -> aux x.arg y.arg && aux x.ret y.ret
      | Sum x, Sum y -> aux x.left y.left && aux x.right y.right
      | Product x, Product y -> aux x.left y.left && aux x.right y.right
      | Rec x, Rec y -> aux x.tau y.tau
      | Forall x, Forall y -> aux x.tau y.tau
      | Exists x, Exists y -> aux x.tau y.tau
      | _ -> false
    in
    aux (to_debruijn tau1) (to_debruijn tau2)
  ;;

  (* let inline_tests () =
     let p = Parser.parse_type_exn in
     assert (equal (substitute "a" (p "num") (p "forall b . a")) (p "forall a . num"));
     assert (equal (substitute "a" (p "b") (p "forall b . a")) (p "forall c . b"));
     assert (not (equal (substitute "a" (p "b") (p "forall b . a")) (p "forall b . b")));
     assert (
     equal
     (substitute "a" (p "b") (p "forall b . forall b . a"))
     (p "forall q . forall c . b"));
     assert (
     not
     (equal
     (substitute "a" (p "b") (p "forall b . forall b . a"))
     (p "forall a . forall b . a")));
     assert (equal (p "forall a . a") (p "forall b . b"));
     assert (not (equal (p "forall a . a") (p "forall b . num")));
     assert (equal (p "forall a . forall b . a -> b") (p "forall x . forall y . x -> y"))
     ;; *)

  (* Uncomment the line below when you want to run the inline tests. *)
  (* let () = inline_tests () *)
end

module Expr = struct
  open Ast.Expr

  let aequiv (e1 : t) (e2 : t) : bool =
    let rec aux (e1 : t) (e2 : t) : bool =
      match e1, e2 with
      | Num n1, Num n2 -> n1 = n2
      | Var x, Var y -> String.equal x y
      | Binop l, Binop r ->
        [%compare.equal: binop] l.binop r.binop
        && aux l.left r.left
        && aux l.right r.right
      | True, True | False, False -> true
      | If l, If r -> aux l.cond r.cond && aux l.then_ r.then_ && aux l.else_ r.else_
      | Relop l, Relop r ->
        [%compare.equal: relop] l.relop r.relop
        && aux l.left r.left
        && aux l.right r.right
      | And l, And r -> aux l.left r.left && aux l.right r.right
      | Or l, Or r -> aux l.left r.left && aux l.right r.right
      | Lam l, Lam r -> aux l.e r.e
      | App l, App r -> aux l.lam r.lam && aux l.arg r.arg
      | Unit, Unit -> true
      | Pair l, Pair r -> aux l.left r.left && aux l.right r.right
      | Project l, Project r -> aux l.e r.e && [%compare.equal: direction] l.d r.d
      | Inject l, Inject r -> aux l.e r.e && [%compare.equal: direction] l.d r.d
      | Case l, Case r -> aux l.e r.e && aux l.eleft r.eleft && aux l.eright r.eright
      | Fix l, Fix r -> aux l.e r.e
      | TyLam l, TyLam r -> aux l.e r.e
      | TyApp l, TyApp r -> aux l.e r.e
      | Fold_ l, Fold_ r -> aux l.e r.e
      | Unfold l, Unfold r -> aux l r
      | Export l, Export r -> aux l.e r.e
      | Import l, Import r -> aux l.e_mod r.e_mod && aux l.e_body r.e_body
      | _ -> false
    in
    aux (to_debruijn e1) (to_debruijn e2)
  ;;

  (* let inline_tests () =
     let p = Parser.parse_expr_exn in
     assert (equal (p "tyfun a -> fun (x : a) -> x") (p "tyfun b -> fun (x : b) -> x"));
     ()
     ;; *)

  (* Uncomment the line below when you want to run the inline tests. *)
  (* let () = inline_tests () *)
end
