(**
 * @since 2015-07-08
 * @author Olle Harstedt
 *)

open Parser_hack
open Infer
open Printf

  (*
let _ =
  SharedMem.(init default_config);
  let file_content = read_file "test.php" in
  let parser_return = program (Relative_path.Root, "") file_content in
  print_endline (Ast.show_program parser_return.ast)
*)


let _ =
  Printexc.record_backtrace true;
  
  let open Parser_hack in
  SharedMem.(init default_config);
  let file_content = Utils.read_file "test.php" in
  let parser_return = Parser_hack.program (Relative_path.Root, "") file_content in
  print_endline (Ast.show_program parser_return.ast);

  let program = infer_program 0 parser_return.ast in
  print_endline "Typedast.show_program";
  printf "%d\n" (List.length program);
  printf "%s\n" (Typedast.show_program program)
