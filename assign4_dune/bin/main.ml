open Lam
open Core
open Result.Monad_infix

let interpret (expr : Ast.Expr.t) =
  if Flags.verbose ()
  then
    Printf.printf
      "Expr: %s\n\n"
      (Ast.Expr.to_string
         (if Flags.testing () then Ast_util.Expr.to_debruijn expr else expr));
  let%bind.Result ty = Typecheck.typecheck expr in
  if Flags.verbose ()
  then
    Printf.printf
      "Type: %s\n\n"
      (Ast.Type.to_string (if Flags.testing () then Ast_util.Type.to_debruijn ty else ty));
  Interpreter.eval expr
;;

let run (filepath : string) =
  let input = In_channel.read_all filepath in
  let result = Parser.parse_expr input >>= interpret in
  match result with
  | Ok e ->
    Printf.printf
      "Success: %s\n"
      (Ast.Expr.to_string (if Flags.testing () then Ast_util.Expr.to_debruijn e else e))
  | Error s -> Printf.printf "Error: %s\n" s
;;

let main () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Lam1 interpreter"
    [%map_open
      let filepath = anon ("filepath" %: string)
      and verbose = flag "v" no_arg ~doc:"Print verbose information"
      and extra_verbose = flag "vv" no_arg ~doc:"Print extra verbose information"
      and testing =
        flag "t" no_arg ~doc:"Print all outputs in format required for test harness"
      in
      fun () ->
        Flags.set_verbose (verbose || extra_verbose);
        Flags.set_extra_verbose extra_verbose;
        Flags.set_testing testing;
        run filepath]
  |> Command_unix.run
;;

let () = main ()
