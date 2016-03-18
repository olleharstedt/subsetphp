(**
 * @since 2015-07-08
 * @author Olle Harstedt
 *)

open Parser_hack
open Infer
open Printf

let _ =
  Printexc.record_backtrace true;
  
  let open Parser_hack in
  SharedMem.(init default_config);

  (* Get command-line argument file *)
  let filename = if Array.length Sys.argv = 1 then
    Sys.argv.(0)
  else
    "test.php"
  in

  let file_content = Utils.read_file filename in
  let parser_return = Parser_hack.program (Relative_path.Root, filename) file_content in

  (* If no error, dump a lot of info *)
  if parser_return.error = None then begin

    print_endline (Ast.show_program parser_return.ast);

    let program = infer_program 0 parser_return.ast in
    print_endline "Typedast.show_program";
    printf "%d\n" (List.length program);
    printf "%s\n" (Typedast.show_program program)
  (* If error, print line and message etc *)
  end else begin
    let pos, msg = match parser_return.error with
      | None ->
          assert false
      | Some (pos, msg) ->
          pos, msg
    in
    let line, start, end_ = Pos.info_pos pos in
    printf "File %S, line %d, characters %d-%d: %s\n"
      (snd Pos.(pos.pos_file)) line start end_ msg
  end
