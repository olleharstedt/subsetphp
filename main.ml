(**
 * @since 2015-07-08
 * @author Olle Harstedt
 *)

open Parser_hack

let _ =
  SharedMem.(init default_config);
  let parser_return = program (Relative_path.Root, "") "
  <?hh
  function fn() {}
  $a = 10 + 25;
  " in
  print_endline (Ast.show_program parser_return.ast)
