(**
 * LLVM-bindings to subsetphp
 *
 * @author Olle Harstedt
 * @since 2015-07-28
 *)

(**
 * Does size of executable matter anymore? With FastCGI, it's
 * only loaded once. When compiling, prioritize speed instead of
 * size. Especially if CPU speed will keep to lag after memory size.
 *
 * Define one function for each possible combination of optional
 * arguments.
 *
 * Hack in strict mode could theoretically be as fast as Java.
 *
 * Compile bitcode files with `llc hello.bc`.
 * Then you need to link the .s file with `clang -c hello.s`, and
 * `clang -o hello hello.o`
 *)

open Llvm
open Typedast

exception Llvm_error of string
exception Llvm_not_implemented of string

(** Setting up LLVM globals needed *)
let llctx = global_context ()
let llm = create_module llctx "mymodule"
let double_type = double_type llctx

(**
 * Generate LLVM IR for program
 *)
let codegen_program program =
  ()

let codegen_def def =
  ()

let codegen_stmt stmt =
  ()

let codegen_expr expr_ = match expr_ with
(*
  | Id (id, ty) -> ()
  | Lvar (id, ty) -> ()
*)
  | Number nr ->
    const_float double_type nr
  | _ -> raise (Llvm_not_implemented "codegen_expr")

let _ =
  (*
  let triple = "i686" in
  let lltarget  = Llvm_target.Target.by_triple triple in
  let llmachine = Llvm_target.TargetMachine.create ~triple:triple lltarget in
  let lldly     = Llvm_target.TargetMachine.data_layout llmachine in
  set_target_triple (Llvm_target.TargetMachine.triple llmachine) llm;
  set_data_layout (Llvm_target.DataLayout.as_string lldly) llm;
  *)

  (* New type int32 *)
  let i32_t = i32_type llctx in
  (* New function type *)
  let fty = function_type i32_t [||] in
  (* New function definition, main *)
  let fn = define_function "main" fty llm in
  (* Create a builder at end of block for function main *)
  let llbuilder = builder_at_end llctx (entry_block fn) in
  (* Build a return of value 0 *)
  (*let _ = build_ret (const_int i32_t 0) llbuilder in*)

  (* New type int8 *)
  let i8_t = i8_type llctx in
  (* New function type with variadic arguments *)
  let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
  (* Declare function printf *)
  let printf = declare_function "printf" printf_ty llm in
  (* Add some attributes to printf *)
  (* No unwind = will not raise an exception *)
  add_function_attr printf Attribute.Nounwind;
  (* No capture = printf makes no copy of the argument that survives printf *)
  add_param_attr (param printf 0) Attribute.Nocapture;
  (* Build pointer to global string *)
  let s = build_global_stringptr "Hello, world!\n" "" llbuilder in
  let zero = const_int i32_t 0 in
  (* GEP = get element pointer *)
  let s = build_in_bounds_gep s [|zero|] "" llbuilder in

  let _ = build_call printf [|s|] "" llbuilder in
  let _ = build_ret (const_int i32_t 0) llbuilder in

  let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10 in

  dump_module llm;

  Llvm_analysis.assert_valid_module llm;

  let _ = Llvm_bitwriter.write_bitcode_file llm "llvm_test.bc" in
  ()
