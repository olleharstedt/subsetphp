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
open Printf

exception Llvm_error of string
exception Llvm_not_implemented of string

(** Setting up LLVM globals needed *)
let llctx = global_context ()
let llm = create_module llctx "mymodule"
let double_type = double_type llctx
let i32_t = i32_type llctx
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10

(** 
 * Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. 
 *
 * From Kaleidoscope tutorial
 *)
let create_entry_block_alloca the_function var_name =
  let builder = builder_at llctx (instr_begin (entry_block the_function)) in
  build_alloca double_type var_name builder

(**
 * Generate LLVM IR for program
 *
 * All "global" PHP variables are wrapped in the main function.
 *
 * @param Typedast.program program
 * @return ?
 *)
let rec codegen_program program =

  (* New function type *)
  let fty = function_type i32_t [||] in
  (* New function definition, main *)
  let fn = define_function "main" fty llm in
  (* Create a builder at end of block for function main *)
  let llbuilder = builder_at_end llctx (entry_block fn) in

  (** Generate list of defs *)
  let rec aux program = match program with
    | [] ->
        ()
    | Stmt stmt :: tail ->
        codegen_stmt stmt llbuilder;
        aux tail
    | Fun fun_ :: tail ->
        ()
  in
  aux program;
  build_ret (const_int i32_t 0) llbuilder

(**
 * Generate LLVM IR for stmt
 *)
and codegen_stmt stmt llbuilder = 
  match stmt with
  | Expr (ty, expr) ->
      codegen_expr expr llbuilder
  | _ -> raise (Llvm_not_implemented "codegen_stmt")

(**
 * Generate LLVM IR for expr
 *)
and codegen_expr expr llbuilder = 
  match expr with
  (*
  | p, Id (id, ty) -> 
      ()
  | p, Lvar (id, ty) -> 
      (*let the_function = block_parent (insertion_block llbuilder) in*)
      ()
  *)
  | p, Number nr ->
      const_float double_type nr;
  | p, Int (pos, i) ->
      print_endline "";
      let f = float_of_string i in
      const_float double_type f;
  (* Assign value to variable *)
  | p, Binop (Eq None, (lhs_pos, Lvar ((lvar_pos, lvar_name), lvar_ty)), value_expr, binop_ty) ->
      let value_expr_code = codegen_expr value_expr llbuilder in
      let the_function = block_parent (insertion_block llbuilder) in
      let variable = try Hashtbl.find named_values lvar_name with
        | Not_found ->
            (* If variable is not found in this scope, create a new one *)
            let alloca = create_entry_block_alloca the_function lvar_name in
            let init_val = const_float double_type 0.0 in
            ignore (build_store init_val alloca llbuilder);
            Hashtbl.add named_values lvar_name alloca;
            alloca
      in
      ignore (build_store value_expr_code variable llbuilder);
      value_expr_code
  | expr -> raise (Llvm_not_implemented (sprintf "codegen_expr: %s" (show_expr expr)))

let _ =
  (*
  let triple = "i686" in
  let lltarget  = Llvm_target.Target.by_triple triple in
  let llmachine = Llvm_target.TargetMachine.create ~triple:triple lltarget in
  let lldly     = Llvm_target.TargetMachine.data_layout llmachine in
  set_target_triple (Llvm_target.TargetMachine.triple llmachine) llm;
  set_data_layout (Llvm_target.DataLayout.as_string lldly) llm;
  *)

  (* Build a return of value 0 *)
  (*let _ = build_ret (const_int i32_t 0) llbuilder in*)

  (*
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
  *)


  let open Parser_hack in
  SharedMem.(init default_config);
  let file_content = Utils.read_file "test.php" in
  let parser_return = Parser_hack.program (Relative_path.Root, "") file_content in
  print_endline (Ast.show_program parser_return.ast);

  let program = Infer.infer_program 0 parser_return.ast in
  printf "%s\n" (Typedast.show_program program);

  ignore (codegen_program program);

  dump_module llm;

  Llvm_analysis.assert_valid_module llm;

  let _ = Llvm_bitwriter.write_bitcode_file llm "llvm_test.bc" in
  ()
