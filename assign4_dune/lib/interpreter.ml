open Core
open Ast

type outcome =
  | Step of Expr.t
  | Val
[@@deriving compare]

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
    (match left, right with
     | Expr.Num n1, Expr.Num n2 ->
       let f =
         match binop with
         | Expr.Add -> ( + )
         | Expr.Sub -> ( - )
         | Expr.Mul -> ( * )
         | Expr.Div -> ( / )
       in
       Step (Expr.Num (f n1 n2))
     | _ -> failwith "The operator for given binop is not type [num]")
  (* Add more cases here! *)
  | _ ->
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

let inline_tests () =
  let p = Parser.parse_expr_exn in
  let e1 = p "2 + 3" in
  assert ([%compare.equal: outcome] (trystep e1) (Step (Expr.Num 5)));
  let e2 = p "(fun (x : num) -> x) 3" in
  assert ([%compare.equal: outcome] (trystep e2) (Step (Expr.Num 3)))
;;

(* Uncomment the line below when you want to run the inline tests. *)
(* TODO move to test *)
(* let () = inline_tests () *)
