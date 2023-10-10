open! Core
open Lam

val check_type : Ast.Expr.t -> Ast.Type.t -> unit
val error_type : Ast.Expr.t -> unit
val check_type_eval : string -> Ast.Type.t -> Ast.Expr.t -> unit
