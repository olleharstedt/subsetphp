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
