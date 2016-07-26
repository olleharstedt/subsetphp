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
    (dum, Atom "module")
  ]))

let _ =
  (*
  let options = [`Shared] in
  let lm = Malfunction_compiler.module_to_lambda ~options test_code in
  *)
  Malfunction_sexp.print Format.str_formatter test_code;
  let str = Format.flush_str_formatter () in
  print_endline str
