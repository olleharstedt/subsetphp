(**
 * LLVM-bindings to subsetphp
 *
 * @author Olle Harstedt
 * @since 2015-07-28
 *)

open Llvm

let _ =
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in
  Llvm.dump_module llm;
  ()
