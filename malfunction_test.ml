(**
 * Try to use malfunction to generate flambda IR instead of LLVM IR
 *
 * Pro: 
 *   No need to make your own GC
 *   Higher abstraction layer, less C
 *   OCaml functions for free (like string buffer, print, etc)
 *
 * Cons:
 *   Might get harder to interface with the PHP runtime? Was it possible in the first place?
 *   Harder to market than LLVM
 *
 * @since 2016-07-24
 * @author Olle Harstedt
 *)

open Typedast
open Malfunction
open Malfunction_sexp
open Lexing

(*
(module
  (_ (apply (global $Pervasives $print_string) "Hello, world!\n"))
  (export))
*)

let dum = (Lexing.dummy_pos, Lexing.dummy_pos)

let test_code =
  (dum, List ([
    (dum, Atom "module");
    (dum, List [
      (dum, Atom "_");
      (dum, List [
          (dum, Atom "apply");
          (dum, List [
              (dum, Atom "global");
              (dum, Atom "$Pervasives");
              (dum, Atom "$print_string")
          ]);
          (dum, String "Hello, world!\n");
      ])
    ]);
    (dum, List [
      (dum, Atom "export");
    ])
  ]))

let _ =
  (*
  let options = [`Shared] in
  let lm = Malfunction_compiler.module_to_lambda ~options test_code in
  *)
  Malfunction_sexp.print Format.str_formatter test_code;
  let str = Format.flush_str_formatter () in
  let e = Malfunction_parser.parse_mod test_code in
  (*
  let l = Malfunction_compiler.module_to_lambda e in
  let c = Malfunction_compiler.lambda_to_cmx "malf_file" "malf_output" l in
  *)
  print_endline str
