(**
 * @since 2015-07-08
 * @author Olle Harstedt
 *)

open Parser_hack

let _ =
  let parser_return = program (Relative_path.Dummy, "") "<?php $a = 10;" in
  ()
