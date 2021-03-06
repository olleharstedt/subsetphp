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
 *
 * Disassemble bitcode with llvm-dis.
 *)

open Llvm
open Llvm_scalar_opts
open Typedast
open Printf

exception Llvm_error of string
exception Llvm_not_implemented of string

(** Setting up LLVM globals needed *)
let llctx = global_context ()
let llm = create_module llctx "mymodule"
let double_type = double_type llctx
let i32_t = i32_type llctx
let i32_ptr_t = pointer_type i32_t
let i64_t = i64_type llctx
let i64_ptr_t = pointer_type i64_t
let i8_t = i8_type llctx
let void_t = void_type llctx
let i8_ptr_t = pointer_type i8_t
let ptr_t = pointer_type i8_t
let ptr_ptr_t = pointer_type ptr_t
let llvm_array_t = named_struct_type llctx "dynamicArray"
(* zend_string opaque type *)
let zend_string_type = named_struct_type llctx "zend_string"
let zend_string_ptr_type = pointer_type zend_string_type
(* ocaml value opaque type *)
let caml_value_type = named_struct_type llctx "caml_value"
let caml_value_ptr_type = pointer_type caml_value_type

(* TODO: Should differ between global and local scope? 
 * Or just clear hash table at new function, as is now. *)
let global_named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10

let zero = const_int i32_t 0

let structs : (string, lltype) Hashtbl.t = Hashtbl.create 10

(* Information for the GC ob how to collect structs (pointer offsets) *)
let structs_gc : (string, struct_) Hashtbl.t = Hashtbl.create 10

(**
 * Get number of field, where 0 is first, 1 second etc
 *
 * @param struct_ struct_
 * @return int
 *)
let get_struct_field_number struct_ field_name =
  let {struct_name; struct_fields} = struct_ in

  (* Has to take into account the struct type-information header *)
  (* Header is two i32_t, so start at 1 (no header = start at -1) *)
  let i = ref (1) in
  let _ = List.find (fun field ->
    i := !i + 1;
    let (name, _) = field in
    name = field_name
  ) struct_fields
  in
  !i

(**
 * Return LLVM type of typed AST type
 *
 * @param Typedast.ty ty
 * @return lltype
 *)
let rec llvm_ty_of_ty ty = match ty with
  | TNumber -> double_type
  | TInt -> i32_t
  | TInt64 -> i64_t
  | TString -> i8_ptr_t
  | TZend_string_ptr -> zend_string_ptr_type
  | TUnit -> void_t
  | TWeak_poly {contents = ((Some TNumber))} -> double_type
  | TWeak_poly {contents = ((Some TString))} -> zend_string_ptr_type
  | TLLVMArray -> llvm_array_t
  | TDynamicSizeArray ty -> 
      let array_struct_type = struct_type llctx [|
        i32_t;  (* Type header *)
        i32_t;
        pointer_type (llvm_ty_of_ty ty);  (* pointer to array *)
        i32_t;  (* used *)
        i32_t;  (* size *)
      |] in
      pointer_type array_struct_type
  | TStruct (class_name, _) -> 
      let class_ty = try Hashtbl.find structs class_name with
          | Not_found -> raise (Llvm_error (sprintf "llvm_ty_of_ty: Class %s was not found" class_name))
      in
      pointer_type class_ty  (* Pointer instead? *)
  | Delayed fn ->
      llvm_ty_of_ty (fn ())
  | _ -> raise (Llvm_not_implemented (sprintf "llvm_ty_of_ty: %s" (show_ty ty)))

(**
 * As above, but TString returns zend_string_ptr_type, not i8_ptr_t for string literals
 *
 * TODO: How to solve this confusion? The inferrer must differ between literals and dynamic strings
 *)
let llvm_ty_of_ty_fun ty = match ty with
  | TNumber -> double_type
  | TInt -> i32_t
  | TString -> zend_string_ptr_type
  | TString_literal -> i8_ptr_t
  | TPtr_ptr -> ptr_ptr_t
  | TPtr -> i8_ptr_t
  | TZend_string_ptr -> zend_string_ptr_type
  | TCaml_value -> caml_value_ptr_type
  | TUnit -> void_t
  | TInt64 -> i64_t
  | TLLVMArray -> llvm_array_t
  | TStruct (class_name, fields) ->
      let class_ty = try Hashtbl.find structs class_name with
          | Not_found -> raise (Llvm_error (sprintf "llvm_ty_of_ty_fun: Class %s was not found" class_name))
      in
      dump_type class_ty;
      pointer_type class_ty  (* Pointer instead? *)

  | TDynamicSizeArray ty -> 
      let array_struct_type = struct_type llctx [|
        i32_t;  (* Type header *)
        i32_t;
        pointer_type (llvm_ty_of_ty ty);  (* pointer to array *)
        i32_t;  (* used *)
        i32_t;  (* size *)
      |] in
      pointer_type array_struct_type
  | _ -> raise (Llvm_not_implemented (sprintf "llvm_ty_of_ty_fun: %s" (show_ty ty)))
(** 
 * Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. 
 *
 * From Kaleidoscope tutorial
 *)
let create_entry_block_alloca the_function var_name ty =
  let builder = builder_at llctx (instr_begin (entry_block the_function)) in
  build_alloca ty var_name builder

(**
 * Create a new alloca with type ty and make it
 * gcroot
 *
 * @param llbuilder
 * @param ty lltype Pointer type, probably
 * @return unit
 *)
let create_new_gcroot_alloca llbuilder ty : llvalue =
  let alloca = build_alloca ty "tmp" llbuilder in
  let tmp = build_bitcast alloca ptr_ptr_t "tmp2" llbuilder in
  let callee =
    match lookup_function "llvm.gcroot" llm with
      | Some callee -> callee
      | None ->
          raise (Llvm_error (sprintf "unknown function referenced: %s" "llvm.gcroot"))
  in
  let args = [|tmp; const_null i8_ptr_t|] in
  build_call callee args "" llbuilder

(**
 * Create a new alloca with type ty and size and make it
 * gcroot
 *
 * Add the header of zend_refcounted, which is uint32_t * 2 = 8 bytes.
 *
 * @param llbuilder
 * @param ty lltype Pointer type, probably
 * @param size llvalue?
 * @return build_call * alloca
 *)
let create_new_gcroot_malloc llbuilder ty size : llvalue * llvalue =
  let alloca = build_alloca ty "tmp" llbuilder in

  (* Zend header - TODO: Correct size? *)
  let zend_refcounted_size = const_int i64_t 64 in

  (* Add header size *)
  let size = const_bitcast size i64_t in
  let add_of_size = build_add size zend_refcounted_size "size" llbuilder in

  (* Call custom gc malloc *)
  let args = [|add_of_size|] in
  let malloc =
    match lookup_function "llvm_gc_allocate" llm with
      | Some callee -> callee
      | None ->
          raise (Llvm_error (sprintf "unknown function referenced: %s" "llvm_gc_allocate"))
  in
  let malloc_result = build_call malloc args "tmp" llbuilder in
  let malloc_result_casted = build_bitcast malloc_result ty "tmp2" llbuilder in
  ignore (build_store malloc_result_casted alloca llbuilder);

  (* Call llvm.gcroot *)
  let tmp = build_bitcast alloca ptr_ptr_t "tmp2" llbuilder in
  let callee =
    match lookup_function "llvm.gcroot" llm with
      | Some callee -> callee
      | None ->
          raise (Llvm_error (sprintf "unknown function referenced: %s" "llvm.gcroot"))
  in
  let args = [|tmp; const_null i8_ptr_t|] in
  build_call callee args "" llbuilder, alloca

(**
 * Generate code for a function prototype
 * From LLVM tutorial
 *
 * There's no support for this in PHP, but still
 * needed for external functions and libraries.
 *
 * @param def def
 * @return llvalue
 *)
let codegen_proto (fun_ : fun_) =
  match fun_ with
  | {f_name = (f_name_pos, name); f_params; f_ret} -> begin

      (* Make the function type: double(double,double) etc. *)
      let args = List.map (fun param -> match param with {param_id; param_type} -> param_type) f_params in
      let args = Array.of_list args in
      let llvm_args = Array.map (fun arg_type ->
          llvm_ty_of_ty_fun arg_type
      ) args in
      let ft = function_type (llvm_ty_of_ty_fun f_ret) llvm_args in
      dump_type ft;
      let f = match lookup_function name llm with
        | None ->
            let name = String.sub name 1 (String.length name - 1) in  (* Strip leading \ (namespace thing) *)
            let fn = declare_function name ft llm in
            set_gc (Some "shadow-stack") fn;
            fn

        (* If 'f' conflicted, there was already something named 'name'. If it
         * has a body, don't allow redefinition or reextern. *)
        | Some f ->

            (* TODO: Allow this? *)

            (* If 'f' already has a body, reject this. *)
            if block_begin f <> At_end f then
              raise (Llvm_error "redefinition of function");

            (* If 'f' took a different number of arguments, reject. *)
            if element_type (type_of f ) <> ft then
              raise (Llvm_error "redefinition of function with different # args");
            f
      in

      (* Set names for all arguments. *)
      Array.iteri (fun i a ->
        let ty = args.(i) in
        let n = string_of_ty ty in
        set_value_name n a;
        Hashtbl.add global_named_values n a;
        ) (params f);
      f
    end

(* Create an alloca for each argument and register the argument in the symbol
 * table so that references to it will succeed. *)
let create_argument_allocas the_function fun_ llbuilder =
  let args = List.map (fun param -> match param with {param_id = (pos, var_name); param_type} -> var_name) fun_.f_params in
  let llvm_args = List.map (fun param -> match param with
    | {param_id; param_type} ->
          llvm_ty_of_ty_fun param_type
  ) fun_.f_params in
  let args = Array.of_list args in
  Array.iteri (fun i ai ->
    let var_name = args.(i) in
    (* TODO: Renaming to avoid name collision does not work
    let (_, f_name) = fun_.f_name in
    let var_name = f_name ^ var_name in
    *)
    let var_type = List.nth llvm_args i in
    (* Create an alloca for this variable. *)
    let alloca = create_entry_block_alloca the_function var_name var_type in

    (* Call gcroot for arg *)
    (* TODO: Only needed for heap allocated args? *)
    (*
    let tmp = build_bitcast alloca ptr_ptr_t "tmp" llbuilder in
    let callee =
      match lookup_function "llvm.gcroot" llm with
        | Some callee -> callee
        | None ->
            raise (Llvm_error (sprintf "unknown function referenced: %s" "llvm.gcroot"))
    in
    let args = [|tmp; const_null i8_ptr_t|] in
    ignore (build_call callee args "" llbuilder);
    *)

    (* Store the initial value into the alloca. *)
    ignore(build_store ai alloca llbuilder);

    (* Add arguments to variable symbol table. *)
    Hashtbl.add global_named_values var_name alloca;
  ) (params the_function)


(**
 * Generate code for function
 * From tutorial
 *
 * @param fun_ fun_
 * @param llbuilder
 * @return llvalue
 *)
let rec codegen_fun (fun_ : fun_) the_fpm =

  print_endline "codegen_fun";

  (* TODO: This means all function must come before "main" script code? *)
  Hashtbl.clear global_named_values;

  let the_function = codegen_proto fun_ in

  (* Create a new basic block to start insertion into. *)
  let bb = append_block llctx "entry" the_function in
  let llbuilder = builder_at_end llctx (entry_block the_function) in
  position_at_end bb llbuilder;

  try
    (* Add all arguments to the symbol table and create their allocas. *)
    create_argument_allocas the_function fun_ llbuilder;

    ignore (codegen_block fun_.f_body llbuilder);

    (* Validate the generated code, checking for consistency. *)
    Llvm_analysis.assert_valid_function the_function;

    (* Optimize the function. *)
    (* TODO: Don't do this here, but in the module, for better error messages. *)
    (*let _ = PassManager.run_function the_function the_fpm in*)

    print_endline "codegen_fun end";
    the_function
  with e ->
    delete_function the_function;
  raise e

(**
 * Generate LLVM IR for program
 *
 * All "global" PHP variables are wrapped in the main function.
 *
 * @param Typedast.program program
 * @return ?
 *)
and codegen_program program =

  let the_fpm = PassManager.create_function llm in

  (* Promote allocas to registers. *)
  add_memory_to_register_promotion the_fpm;

  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  add_instruction_combination the_fpm;

  (* reassociate expressions. *)
  add_reassociation the_fpm;

  (* Eliminate Common SubExpressions. *)
  add_gvn the_fpm;

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  add_cfg_simplification the_fpm;

  ignore (PassManager.initialize the_fpm);

  (** Generate classes *)
  let rec aux_class program = match program with
    | [] ->
        ()
        (*
    | Class class_ :: tail ->
        let _ = codegen_class fun_ the_fpm in
        aux_class tail
        *)
    | Struct struct_ :: tail ->
        codegen_struct struct_ the_fpm;
        aux_class tail
    | somethingelse :: tail ->
        aux_class tail
  in
  aux_class program;

  (** Generate functions *)
  let rec aux_fun program = match program with
    | [] ->
        ()
    | Fun fun_ :: tail ->
        let _ = codegen_fun fun_ the_fpm in
        aux_fun tail
    | somethingelse :: tail ->
        aux_fun tail
  in
  aux_fun program;

  (* Better clear this... *)
  Hashtbl.clear global_named_values;

  (* New function type for the main function *)
  let fty = function_type i32_t [||] in
  (* New function definition, main *)
  let fn = define_function "main" fty llm in
  set_gc (Some "shadow-stack") fn;
  (* Create a builder at end of block for function main *)
  let llbuilder = builder_at_end llctx (entry_block fn) in

  (** Init the GC *)
  let callee =
    (*match lookup_function "subsetphp_gc_init" llm with*)
    match lookup_function "llvm_gc_initialize" llm with
      | Some callee -> callee
      | None ->
          raise (Llvm_error (sprintf "unknown function referenced: %s" "llvm_gc_initialize"))
  in
  let heapsize = const_int i32_t 100000 in
  ignore (build_call callee [|heapsize|] "" llbuilder);

  (** Generate list of defs *)
  let rec aux program = match program with
    | [] ->
        ()
    | Stmt Noop :: tail ->
        (* Don't generate anything for Noop *)
        aux tail
    | Stmt stmt :: tail ->
        let _ = codegen_stmt stmt llbuilder in
        aux tail
    | possibly_fun :: tail ->
        aux tail
  in
  aux program;
  let _ = build_ret (const_int i32_t 0) llbuilder in

  (** Generate GC runtime type information *)
  (*let begin_pos = global_begin llm in*)
  (*instr_begin*)
  (*position_builder begin_pos llbuilder;*)

  generate_gc_runtime_type_information llbuilder;
  (*generate_json_gc_type_information ();*)
  ()

(**
 * Second try to generate type-information for the GC
 * This time using JSON serialization and store it into
 * an external file which is read by the GC at program
 * initialisation.
 *)
and generate_json_gc_type_information () =
  ()

(**
 * The GC needs to know what pointers in a struct
 * to follow and mark/sweep.
 *
 * { type : int; nr_of_offsets : int; pointer_offsets : int array }
 *
 * @param llbuilder
 * @return unit
 *)
and generate_gc_runtime_type_information llbuilder =

  print_endline "1";
  let nr_of_struct_types = Hashtbl.length structs in
  print_endline "2";
  printf "nr_of_struct_types = %d\n" nr_of_struct_types;
  (*let pointer_offsets_t = array_type i32_t 2 in*)
  print_endline "3";
  let array_of_ptrs = Array.make nr_of_struct_types (const_pointer_null i8_ptr_t) in
  print_endline "6";
  let j = ref 0 in
  print_endline "7";

  (*let t1 = struct_type*)

  (**
   * Generate a bunch of global structs with
   * type information.
   *
   * @param struct_type ?
   * @return ?
   *)
  let generate_struct_type_info name struct_type_ =
    begin match struct_type_ with
    | {struct_name = name; struct_fields = fields} ->

        (* Number of heap allocated fields that needs to be collected *)
        let nr_of_gc_fields = ref 0 in

        (* List of offsets for above fields, so the GC knows where to look *)
        let list_of_gc_offsets = ref [] in

        (* Iterate through fields and catch all heap allocated dito *)
        List.iter (fun fi -> match fi with
        | (field_name, Typedast.TWeak_poly {contents = Some Typedast.TString}) ->
            print_endline ("here " ^ field_name);
            list_of_gc_offsets := !list_of_gc_offsets @ [(const_int i32_t !nr_of_gc_fields)];
            printf "offset = %d\n" !nr_of_gc_fields;
            nr_of_gc_fields := !nr_of_gc_fields + 1;
        | (field_name, _)  ->
            (* Not a heap allocation *)
            ()
        ) fields;

        let name = String.sub name 1 (String.length name - 1) in  (* Strip leading \ (namespace thing) *)
        let const = const_struct llctx [|
          (* Number of fields that need to be collected = length of array *)
          const_int i32_t !nr_of_gc_fields;
          (* Array of offsets *)
          const_array i32_t (Array.of_list !list_of_gc_offsets)
        |] in
        let const_ptr = define_global ("structs_gc_info_" ^ name) const llm in
        let ptr = const_pointercast const_ptr i8_ptr_t in
        let glob_ptr = define_global ("structs_gc_info_ptr_" ^ name) ptr llm in
        array_of_ptrs.(!j) <- glob_ptr;
        j := !j + 1
    end
  in

  Hashtbl.iter generate_struct_type_info structs_gc;

  print_endline "18";
  dump_type ptr_ptr_t;
  let const = const_array ptr_ptr_t array_of_ptrs in (* struct has type {i32, i32} *)
  print_endline "19";
  ignore (define_global "structs_gc_info" const llm);
  print_endline "20"

(**
 * Generate LLVM IR for block (stmt list)
 *
 * @param block stmt list
 * @param llbuilder
 * @return llvalue
 *)
and codegen_block block llbuilder : llvalue =

  let the_block = block_parent (insertion_block llbuilder) in

  begin match block with
    | Noop :: [] ->
        ()
    | block ->

        let stmt_values = ref [] in
        for i = 0 to List.length block - 1 do
          let stmt = List.nth block i in
          let stmt_value = codegen_stmt stmt llbuilder in
          stmt_values := !stmt_values @ [stmt_value];
        done;
  end;

  (*stmt_values*)
  the_block

(**
 * Generate code for struct
 * Actually just stores type information for this struct
 * type name.
 *
 * Structs in subsetphp are final classes with only
 * public member variables.
 *
 * @param struct_
 * @param llbuilder
 * @return llvalue
 *)
and codegen_struct struct_ llbuilder : unit =
  print_endline "codegen_struct";
  match struct_ with
  | {struct_name = name; struct_fields = fields} ->
      (* Extract llvm types from fields *)
      let fields = List.map (fun field ->
        match field with
        | (field_name, field_ty) -> llvm_ty_of_ty field_ty
      ) fields in

      (* Add header with type-information for GC *)
      (* For now just two i32 - how many bytes is that? *)
      let fields = i32_t :: fields in
      let fields = i32_t :: fields in
      let fields = Array.of_list fields in

      let name = String.sub name 1 (String.length name - 1) in  (* Strip leading \ (namespace thing) *)
      let t = struct_type llctx fields in
      Hashtbl.add structs name t;
      Hashtbl.add structs_gc name struct_;

(**
 * Generate LLVM IR for stmt
 *
 * @param stmt Typedast.stmt
 * @param llbuilder
 * @return llvalue
 *)
and codegen_stmt (stmt : stmt ) llbuilder : llvalue =
  match stmt with
  | Block block ->
      codegen_block block llbuilder
  | Expr (ty, expr) ->
      codegen_expr expr llbuilder
  | Return (pos, expr_opt, ty) ->
      begin match expr_opt with
        | None ->
            build_ret_void llbuilder
        | Some expr ->
            let expr = codegen_expr expr llbuilder in
            build_ret expr llbuilder
      end
  | If (expr, then_, else_) ->
      let expr = codegen_expr expr llbuilder in

      (* Convert condition to a bool by comparing equal to 0.0 *)
      let zero = const_float double_type 0.0 in
      let cond_val = build_fcmp Fcmp.One expr zero "ifcond" llbuilder in

      (* Grab the first block so that we might later add the conditional branch
       * to it at the end of the function. *)
      let start_bb = insertion_block llbuilder in
      let the_function = block_parent start_bb in

      let then_bb = append_block llctx "then" the_function in

      (* Emit 'then' value. *)
      position_at_end then_bb llbuilder;
      let then_val = codegen_block then_ llbuilder in

      (* Codegen of 'then' can change the current block, update then_bb for the
       * phi. We create a new name because one is used for the phi node, and the
       * other is used for the conditional branch. *)
      let new_then_bb = insertion_block llbuilder in

      (* Emit 'else' value. *)
      let else_bb = append_block llctx "else" the_function in
      position_at_end else_bb llbuilder;
      let else_val = codegen_block else_ llbuilder in

      (* Codegen of 'else' can change the current block, update else_bb for the
       * phi. *)
      let new_else_bb = insertion_block llbuilder in

      (* Emit merge block. *)
      let merge_bb = append_block llctx "ifcont" the_function in
      position_at_end merge_bb llbuilder;
      let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
      let phi = build_phi incoming "iftmp" llbuilder in

      (* Return to the start block to add the conditional branch. *)
      position_at_end start_bb llbuilder;
      ignore (build_cond_br cond_val then_bb else_bb llbuilder);

      (* Set a unconditional branch at the end of the 'then' block and the
       * 'else' block to the 'merge' block. *)
      position_at_end new_then_bb llbuilder; ignore (build_br merge_bb llbuilder);
      position_at_end new_else_bb llbuilder; ignore (build_br merge_bb llbuilder);

      (* Finally, set the llbuilder to the end of the merge block. *)
      position_at_end merge_bb llbuilder;

      phi

      (*
         *)

  | For ( (p1, Binop ((Eq None), (p2, Lvar ((p3, var_name), TNumber)), (p4, (Lvar _)), TUnit)) as start, end_, step, body)
  | For ( (p1, Binop ((Eq None), (p2, Lvar ((p3, var_name), TNumber)), (p4, (Int _)), TUnit)) as start, end_, step, body) ->
      (* Output this as:
       *   var = alloca double
       *   ...
       *   start = startexpr
       *   store start -> var
       *   goto loop
       * loop:
       *   ...
       *   bodyexpr
       *   ...
       * loopend:
       *   step = stepexpr
       *   endcond = endexpr
       *
       *   curvar = load var
       *   nextvar = curvar + step
       *   store nextvar -> var
       *   br endcond, loop, endloop
       * outloop: *)

      let the_function = block_parent (insertion_block llbuilder) in

      (* Create an alloca for the variable in the entry block. *)
      let alloca = create_entry_block_alloca the_function var_name double_type in

      (* Emit the start code first, without 'variable' in scope. *)
      let start_val = codegen_expr start llbuilder in

      (* Store the value into the alloca. *)
      ignore (build_store start_val alloca llbuilder);

      (* Make the new basic block for the loop header, inserting after current
      *  block. *)
      let loop_bb = append_block llctx "loop" the_function in

      (* Insert an explicit fall through from the current block to the
       * loop_bb. *)
      ignore (build_br loop_bb llbuilder);

      (* Start insertion in loop_bb. *)
      position_at_end loop_bb llbuilder;

      (* Within the loop, the variable is defined equal to the PHI node. If it
       * shadows an existing variable, we have to restore it, so save it
       * now. *)
      let old_val =
      try Some (Hashtbl.find global_named_values var_name) with Not_found -> None
      in
      Hashtbl.add global_named_values var_name alloca;

      (* Emit the body of the loop.  This, like any other expr, can change the
       * current BB.  Note that we ignore the value computed by the body, but
       * don't allow an error *)
      ignore (codegen_block body llbuilder);

      (* Emit the step value. *)
      let step_val = codegen_expr step llbuilder in

      (* Compute the end condition. *)
      let end_cond = codegen_expr end_ llbuilder in

      (* Reload, increment, and restore the alloca. This handles the case where
       * the body of the loop mutates the variable. *)
      (*let cur_var = build_load alloca var_name llbuilder in*)
      (*let next_var = build_fadd cur_var step_val "nextvar" llbuilder in*)
      ignore (build_store step_val alloca llbuilder);

      (* Convert condition to a bool by comparing equal to 0.0. *)
      let zero = const_float double_type 0.0 in
      let end_cond = build_fcmp Fcmp.One end_cond zero "loopcond" llbuilder in

      (* Create the "after loop" block and insert it. *)
      let after_bb = append_block llctx "afterloop" the_function in

      (* Insert the conditional branch into the end of loop_end_bb. *)
      ignore (build_cond_br end_cond loop_bb after_bb llbuilder);

      (* Any new code will be inserted in after_bb. *)
      position_at_end after_bb llbuilder;

      (* Restore the unshadowed variable. *)
      (* TODO: Not relevant for PHP, fix. *)
      begin match old_val with
        | Some old_val -> Hashtbl.add global_named_values var_name old_val
        | None -> ()
      end;

      (* for expr always returns 0.0. *)
      const_null double_type

  | _ -> raise (Llvm_not_implemented (sprintf "codegen_stmt: %s" (show_stmt stmt)))

(**
 * Generate LLVM IR for expr
 *
 * @param expr Typedast.expr
 * @param llbuilder
 * @return llvalue
 *)
and codegen_expr (expr : expr) llbuilder : llvalue =
  match expr with
  (*
  | p, Id (id, ty) ->
      ()
  *)
  | p, True ->
      const_float double_type 1.0
  | p, False ->
      const_float double_type 0.0
  | p, Lvar ((pos, lvar_name), ty) ->
      (*print_endline lvar_name;*)
      (*let the_function = block_parent (insertion_block llbuilder) in*)
      let variable = try Hashtbl.find global_named_values lvar_name with
        | Not_found ->
            let line, start, end_ = Pos.info_pos pos in
            let pos_info = sprintf "File %S, line %d, characters %d-%d"
              (snd Pos.(pos.pos_file)) line start end_ in
            raise (Llvm_error (sprintf "Lvar is used on rhs before ever used on the lhs: %s, %s" lvar_name (pos_info)))
      in
      build_load variable lvar_name llbuilder
  | p, Number nr ->
      const_float double_type nr;
  | p, String (pos, str) ->
      (* Create temporary variable to store string in *)
      let alloca = build_alloca zend_string_ptr_type "tmp" llbuilder in
      let tmp = build_bitcast alloca ptr_ptr_t "tmp2" llbuilder in
      let callee =
        match lookup_function "llvm.gcroot" llm with
          | Some callee -> callee
          | None ->
              raise (Llvm_error (sprintf "unknown function referenced: %s" "llvm.gcroot"))
      in
      let args = [|tmp; const_null i8_ptr_t|] in
      ignore (build_call callee args "" llbuilder);

      let str_ptr = build_global_stringptr str str llbuilder in
      let length = const_int i32_t (String.length str) in
      let persistent = const_int i32_t 1 in
      let args = [|str_ptr; length; persistent|] in

      (* Init string with subsetphp_string_init *)
      let callee =
        match lookup_function "subsetphp_string_init" llm with
          | Some callee -> callee
          | None ->
              raise (Llvm_error (sprintf "unknown function referenced: %s" "subsetphp_string_init"))
      in
      let result = build_call callee args "str" llbuilder in
      ignore(build_store result alloca llbuilder);
      result

  | p, Int (pos, i)
  | p, Float (pos, i) ->
      let f = float_of_string i in
      const_float double_type f;

  (* TODO: Not used? *)
  | p, Array (array_values, TFixedSizeArray (array_length, the_array_type)) ->
      let llvm_array_type = array_type (llvm_ty_of_ty the_array_type) array_length in
      (* An array alloca is released when function returns ("stack allocation") *)
      (* Need to use malloc and gc *)

      (*build_array_alloca llvm_array_type (const_int i32_t array_length) "arr" llbuilder*)

      (*build_alloca llvm_array_type "arr" llbuilder*)
      print_endline "type:";
      let build, alloca = create_new_gcroot_malloc llbuilder (pointer_type llvm_array_type) (size_of llvm_array_type) in
      print_endline "build:";
      print_endline "alloca:";
      alloca

  (* Like $array[1] *)
  | p, ArrayFixedSize_get (
        (pos1, Lvar ((pos2, array_var_name), var_type)),
        (pos3, Number index),
        element_type) ->

      let arr : llvalue = try Hashtbl.find global_named_values array_var_name with
        | Not_found -> raise (Llvm_error (sprintf "Tried to load number from struct variable that doesn't exist: %s" array_var_name))
      in
      let loaded_array = build_load arr "loaded_arr" llbuilder in

      let index = int_of_float index in

      let gep = build_struct_gep loaded_array index "gep" llbuilder in
      let load = build_load gep "load" llbuilder in
      load

  (* Like $array[$i] *)
  | p, ArrayFixedSize_get (
        (pos1, Lvar ((pos2, array_var_name), var_type)),
        (pos3, Lvar ((pos4, lvar_name), TNumber)),
        element_type) ->

      (*let struct_ty = Hashtbl.find structs struct_name in*)
      let arr : llvalue = try Hashtbl.find global_named_values array_var_name with
        | Not_found -> raise (Llvm_error (sprintf "Tried to load number from struct variable that doesn't exist: %s" array_var_name))
      in
      let loaded_array = build_load arr "loaded_arr" llbuilder in

      let index_expr = codegen_expr (pos3, Lvar ((pos4, lvar_name), TNumber)) llbuilder in
      let index = build_fptoui index_expr i32_t "index" llbuilder in

      let gep = build_gep loaded_array [|zero; index|] "gep" llbuilder in
      let load = build_load gep "load" llbuilder in
      load

  (* Like $array[0] with dynamically sized array *)
  | p, ArrayDynamicSize_get (
        (pos1, Lvar ((pos2, array_var_name), var_type)),
        (pos3, Number index),
        element_type) ->

      let arr : llvalue = try Hashtbl.find global_named_values array_var_name with
        | Not_found -> raise (Llvm_error (sprintf "Tried to load number from struct variable that doesn't exist: %s" array_var_name))
      in
      let loaded_array_struct = build_load arr "loaded_array_struct" llbuilder in

      (* TODO: Add bounds check Throw exception? PHP notice: Undefined offset x in file y, line z *)
      (* Why index 2 here? Because that's the address of the array. See the struct
       * def in bindings2.c:
       *)
      let gep = build_struct_gep loaded_array_struct 2 "gep" llbuilder in
      let loaded_array = build_load gep "loaded_array" llbuilder in

      let index = int_of_float index in
      let llvm_index = const_int i32_t index in
      let gep = build_gep loaded_array [|llvm_index|] "gep" llbuilder in
      let load = build_load gep "load" llbuilder in
      load

  | p, Unop (Uminus expr, TNumber) ->
      let expr_code = codegen_expr expr llbuilder in
      build_fneg expr_code "neg" llbuilder

  (* < *)
  | p, Binop (Lt, lexpr, rexpr, TBoolean) ->

      let lexpr_code = codegen_expr lexpr llbuilder in
      let rexpr_code = codegen_expr rexpr llbuilder in

      let i = build_fcmp Fcmp.Olt lexpr_code rexpr_code "EQeqeq" llbuilder in
      build_uitofp i double_type "booltmp" llbuilder

  (* > *)
  | p, Binop (Gt, lexpr, rexpr, TBoolean) ->

      let lexpr_code = codegen_expr lexpr llbuilder in
      let rexpr_code = codegen_expr rexpr llbuilder in

      let i = build_fcmp Fcmp.Ogt lexpr_code rexpr_code "EQeqeq" llbuilder in
      build_uitofp i double_type "booltmp" llbuilder

  (* === on numbers *)
  | p, Binop (EQeqeq TNumber, lexpr, rexpr, TBoolean) ->

      let lexpr_code = codegen_expr lexpr llbuilder in
      let rexpr_code = codegen_expr rexpr llbuilder in

      let i = build_fcmp Fcmp.Oeq lexpr_code rexpr_code "EQeqeq" llbuilder in
      build_uitofp i double_type "booltmp" llbuilder

  (* +=, only allowed on numbers *)
  | p, Binop (Eq (Some Plus), (lha_pos, Lvar ((lvar_pos, lvar_name), TNumber)), rexpr, TNumber) ->
      (*let the_function = block_parent (insertion_block llbuilder) in*)
      let variable = try Hashtbl.find global_named_values lvar_name with
        | Not_found ->
            (* Should not happen *)
            failwith "Tried to use += on variable that is not defined"
        in
      let rexpr_code = codegen_expr rexpr llbuilder in
      let load_code = build_load variable lvar_name llbuilder in
      let add_code = build_fadd load_code rexpr_code "addtmp" llbuilder in

      ignore (build_store add_code variable llbuilder);
      add_code

  (* -=, only allowed on numbers *)
  | p, Binop (Eq (Some Minus), (lha_pos, Lvar ((lvar_pos, lvar_name), TNumber)), rexpr, TNumber) ->
      (*let the_function = block_parent (insertion_block llbuilder) in*)
      let variable = try Hashtbl.find global_named_values lvar_name with
        | Not_found ->
            (* Should not happen *)
            failwith "Tried to use -= on variable that is not defined"
        in
      let rexpr_code = codegen_expr rexpr llbuilder in
      let load_code = build_load variable lvar_name llbuilder in
      let add_code = build_fsub load_code rexpr_code "addtmp" llbuilder in

      ignore (build_store add_code variable llbuilder);
      add_code

  (* In case type of lvar is delayed (see note in ty_of_ty in infer.ml) *)
  | p, Binop (Eq None, (lhs_pos, Lvar (lvar, Delayed fn)), value_expr, binop_ty) ->
      let typ = fn () in

      (* This new expression is same as above but with delayed type get *)
      let new_expr = (p, Binop (Eq None, (lhs_pos, Lvar (lvar, typ)), value_expr, binop_ty)) in
      codegen_expr new_expr llbuilder

  (* Assign number to variable *)
  | p, Binop (Eq None, (lhs_pos, Lvar ((lvar_pos, lvar_name), TNumber)), value_expr, binop_ty) ->
      let the_function = block_parent (insertion_block llbuilder) in
      let variable = try Hashtbl.find global_named_values lvar_name with
        | Not_found ->
            print_endline "new variable in scope";
            (* If variable is not found in this scope, create a new one *)
            let alloca = create_entry_block_alloca the_function lvar_name double_type in
            let init_val =  const_float double_type 0.0 in
            ignore (build_store init_val alloca llbuilder);
            Hashtbl.add global_named_values lvar_name alloca;
            alloca
            in
      let value_expr_code = codegen_expr value_expr llbuilder in
      let store = build_store value_expr_code variable llbuilder in
      ignore (store);
      value_expr_code

  (* Create new struct, like $p = new Point() *)
  | (p,
       Binop ((Eq None),
         (pos1, Lvar ((pos2, lvar_name), lvar_type)),
         (pos3,
          New (
            (pos4,
             Id ((pos5, struct_type_name), struct_type
               )
             ),
            [], [], TUnknown)), TUnit)) ->
      print_endline "create new struct";
      let ty = try Hashtbl.find structs struct_type_name with
      | Not_found ->
          failwith ("Could not find any struct type " ^ struct_type_name ^ ": " ^ Infer.get_pos_msg p)
      in
      let build, alloca = create_new_gcroot_malloc llbuilder (pointer_type ty) (size_of ty) in
      Hashtbl.add global_named_values lvar_name alloca;
      build

  (* Create new array, like $arr = [1, 2, 3] *)
  | p,
       Binop ((Eq None),
         (pos2,
          Lvar ((pos3, var_name), var_type)),
         (pos4,
          Array (array_fields,
            TFixedSizeArray (arr_length, arr_elem_type))), TUnit) ->
              
      print_endline "create new array";
      let llvm_array_type = array_type (llvm_ty_of_ty arr_elem_type) arr_length in
      let build, alloca = create_new_gcroot_malloc llbuilder (pointer_type llvm_array_type) (size_of llvm_array_type) in
      Hashtbl.add global_named_values var_name alloca;

      (* Loop through fields and store them in array *)
      let loaded_array = build_load alloca "loaded_arr" llbuilder in
      for i = 0 to arr_length - 1 do
        match (List.nth array_fields i) with
        | AFvalue expr ->
            let exp = codegen_expr expr llbuilder in
            let gep = build_struct_gep loaded_array i "gep" llbuilder in
            ignore (build_store exp gep llbuilder);
      done;

      build

  (* Initialise new dynamic array, like $arr = [] *)
  | (p,
     Binop ((Eq None),
       (pos2,
        Lvar ((pos3, lvar_name),
          (TDynamicSizeArray (Delayed delayed_ty)))),
       (pos4,
        Array ([], dyn_arr_ty)),  (* array type is always unknown here, like TDynamicSizeArray TUnresolved *)
       TUnit)) ->
     let ty = delayed_ty () in
     let var_type = TDynamicSizeArray ty in

     (* Create array with C *)
     (* initArray *)
     zero

  (* Assign struct to variable, like $b = $point *)
  (* TODO: Code-duplication with string assign below *)
  | p, Binop (Eq None, (lhs_pos, Lvar ((lvar_pos, lvar_name), TStruct (struct_name, struct_fields))), value_expr, binop_ty) ->
      print_endline "assign struct to variable";
      let the_function = block_parent (insertion_block llbuilder) in
      let variable = try Hashtbl.find global_named_values lvar_name with
        | Not_found ->
            (* If variable is not found in this scope, create a new one *)
            let struct_ty = Hashtbl.find structs struct_name in
            let builder = builder_at llctx (instr_begin (entry_block the_function)) in
            let alloca = build_alloca (pointer_type struct_ty) lvar_name builder in
            let tmp = build_bitcast alloca ptr_ptr_t "tmp" builder in
            let callee =
              match lookup_function "llvm.gcroot" llm with
                | Some callee -> callee
                | None ->
                    raise (Llvm_error (sprintf "unknown function referenced: %s" "llvm.gcroot"))
            in
            let args = [|tmp; const_null i8_ptr_t|] in
            ignore (build_call callee args "" llbuilder);

            Hashtbl.add global_named_values lvar_name alloca;
            alloca
      in
      let value_expr_code = codegen_expr value_expr llbuilder in
      ignore (build_store value_expr_code variable llbuilder);
      value_expr_code

  (* Assign string to variable *)
  | p, Binop (Eq None, (lhs_pos, Lvar ((lvar_pos, lvar_name), TString)), value_expr, binop_ty) ->
      let the_function = block_parent (insertion_block llbuilder) in
      let variable = try Hashtbl.find global_named_values lvar_name with
        | Not_found ->
            (* If variable is not found in this scope, create a new one *)
            let builder = builder_at llctx (instr_begin (entry_block the_function)) in
            let alloca = build_alloca zend_string_ptr_type lvar_name builder in
            let tmp = build_bitcast alloca ptr_ptr_t "tmp" builder in
            let callee =
              match lookup_function "llvm.gcroot" llm with
                | Some callee -> callee
                | None ->
                    raise (Llvm_error (sprintf "unknown function referenced: %s" "llvm.gcroot"))
            in
            let args = [|tmp; const_null i8_ptr_t|] in
            ignore (build_call callee args "" llbuilder);

            Hashtbl.add global_named_values lvar_name alloca;
            alloca
      in
      let value_expr_code = codegen_expr value_expr llbuilder in
      ignore (build_store value_expr_code variable llbuilder);
      value_expr_code

  (** 
   * Assign array to variable 
   *
   * $arr = [1, 2, 3]
   *)
  | p, Typedast.Binop ((Typedast.Eq None),
    (lhs_pos, Typedast.Lvar ((lvar_pos, lvar_name), Typedast.TFixedSizeArray (array_length, the_array_type))),
    (value_expr),
      Typedast.TUnit)
     ->
       print_endline "assign array to variable";
      let the_function = block_parent (insertion_block llbuilder) in
      let variable = try Hashtbl.find global_named_values lvar_name with
        | Not_found ->
            (* If variable is not found in this scope, create a new one *)
            let builder = builder_at llctx (instr_begin (entry_block the_function)) in
            let llvm_array_type = array_type (llvm_ty_of_ty the_array_type) array_length in
            (*let llvm_length = const_int i32_t array_length in*)
            let alloca = build_alloca (pointer_type llvm_array_type) lvar_name builder in
            let tmp = build_bitcast alloca ptr_ptr_t "tmp" builder in
            let callee =
              match lookup_function "llvm.gcroot" llm with
                | Some callee -> callee
                | None ->
                    raise (Llvm_error (sprintf "unknown function referenced: %s" "llvm.gcroot"))
            in
            let args = [|tmp; const_null i8_ptr_t|] in
            ignore (build_call callee args "" llbuilder);

            Hashtbl.add global_named_values lvar_name alloca;
            alloca
      in
      let value_expr_code = codegen_expr value_expr llbuilder in
      let store_expr = build_store value_expr_code variable llbuilder in
      ignore (store_expr);
      value_expr_code

  (* Assign int/float to struct 
   * 
   * PHP:
   *   $struct->x = 10;
   * 
   * LLVM:
   *   %Foo = type {i32, i32}
   *   %foo = alloca %Foo
   *   %0 = getelementptr %Foo* %foo, i32 0, i32 1
   *   store i32 10, i32* %0
   *)
  | (p, Typedast.Binop ((Typedast.Eq None),
         (pos1,
          Typedast.Obj_get (
            (pos2,
             Typedast.Lvar ((pos3, lvar_name),
               (Typedast.TStruct (struct_name, fields))
               )
             ),
            (pos4, Typedast.Id ((pos5, field_name), Typedast.TNumber)),
            Typedast.OG_nullthrows, Typedast.TUnit)),
         (pos6, value_expr), Typedast.TUnit)) ->

      (*let the_function = block_parent (insertion_block llbuilder) in*)
      let stru = try Hashtbl.find global_named_values lvar_name with
        | Not_found -> raise (Llvm_error (sprintf "Tried to assign number to struct variable that doesn't exist: %s" lvar_name))
      in
      print_endline "here";

      (* Get struct type, cast pointer to this type, then get gep *)
      let struct_ty = Hashtbl.find structs struct_name in
      dump_type struct_ty;
      let loaded_stru = build_load stru "loaded_stru" llbuilder in
      (*let cast_stru = build_bitcast stru (pointer_type struct_ty) "tmp2" llbuilder in*)
      let struct_ = Hashtbl.find structs_gc struct_name in
      let field_number = get_struct_field_number struct_ field_name in
      let gep = build_struct_gep loaded_stru field_number "gep" llbuilder in

      let number_expr = codegen_expr (pos6, value_expr) llbuilder in

      ignore(build_store (number_expr) gep llbuilder);
      zero (* Should be unit *)


  (* Get int/variable from struct
   *
   * PHP:
   *   $x = $struct->x
   * 
   * LLVM:
   *   %struct.Foo = type { double, double }
   *     %foo = alloca %struct.Foo, align 8
   *     %x = alloca double, align 8
   *     %1 = getelementptr inbounds %struct.Foo* %foo, i32 0, i32 0
   *     %2 = load double* %1, align 8
   *     store double %2, double* %x, align 8
   *     ret i32 0
   *)
  | p, Typedast.Obj_get (
        (pos1, Typedast.Lvar ((pos2, struct_var_name), Typedast.TStruct (struct_name, struct_fields))),
        (pos3, Typedast.Lvar ((pos4, struct_field_name), struct_field_ty)),
        Typedast.OG_nullthrows,
        result_ty) ->

      (*let struct_ty = Hashtbl.find structs struct_name in*)
      let stru : llvalue = try Hashtbl.find global_named_values struct_var_name with
        | Not_found -> raise (Llvm_error (sprintf "Tried to load number from struct variable that doesn't exist: %s" struct_var_name))
      in
      (*let cast_stru = build_bitcast stru (pointer_type struct_ty) "tmp2" llbuilder in*)
      let loaded_stru = build_load stru "loaded_stru" llbuilder in

      let struct_ = Hashtbl.find structs_gc struct_name in
      let field_number = get_struct_field_number struct_ struct_field_name in

      let gep = build_struct_gep loaded_stru field_number "gep" llbuilder in
      let load = build_load gep "load" llbuilder in
      load

  (* Assign whatever to object member variable 
   * OBS: Don't move this above create new struct. The pattern-matching is to generous.
   *)
  | p, Binop ((Typedast.Eq None), (pos1, Typedast.Obj_get (a, c, d, e)), b, TUnit) ->
      raise (Llvm_not_implemented (sprintf "Assign to member variable: %s to %s" (show_expr b) (show_expr (pos1, Typedast.Obj_get (a, c, d, e)))))

  (* Numerical operations *)
  | p, Binop (bop, expr1, expr2, binop_ty) when is_numerical_op bop ->
      let lhs = codegen_expr expr1 llbuilder in
      let rhs = codegen_expr expr2 llbuilder in
      begin match bop with
        | Plus ->
            build_fadd lhs rhs "addtmp" llbuilder
        | Minus ->
            build_fsub lhs rhs "subtmp" llbuilder
        | Star ->
            build_fmul lhs rhs "multmp" llbuilder
        | Slash ->
            build_fdiv lhs rhs "multmp" llbuilder
        | bop -> raise (Llvm_not_implemented (show_bop bop))
      end;

  (* . concatenation *)
  | p, Typedast.Binop (Typedast.Dot, expr1, expr2, TString) ->
  (*| p, Typedast.Binop (Typedast.Dot, (p, Typedast.Lvar ((p, "$a"), Typedast.TString)), (p, (Typedast.String (p, "qwe"))), Typedast.TString) ->*)

      (* Both lhs and rhs should be zend string pointers *)
      let lhs = codegen_expr expr1 llbuilder in
      let rhs = codegen_expr expr2 llbuilder in

      (* Create temporary variable to store string in *)
      let alloca = build_alloca (zend_string_ptr_type) "tmp" llbuilder in
      let tmp = build_bitcast alloca ptr_ptr_t "tmp2" llbuilder in
      let callee =
        match lookup_function "llvm.gcroot" llm with
          | Some callee -> callee
          | None ->
              raise (Llvm_error (sprintf "unknown function referenced: %s" "llvm.gcroot"))
      in
      let args2 = [|tmp; const_null i8_ptr_t|] in
      ignore (build_call callee args2 "" llbuilder);

      (* Call subsetphp_concat_function *)
      let callee =
        match lookup_function "subsetphp_concat_function" llm with
          | Some callee -> callee
          | None ->
              raise (Llvm_error (sprintf "unknown function referenced: %s" "subsetphp_concat_function"))
      in
      let args = [|lhs; rhs;|] in
      (*build_call callee args "str" llbuilder*)
      let result = build_call callee args "str" llbuilder in
      ignore(build_store result alloca llbuilder);
      result

  (* Function call that return unit *)
  | p, Call ((pos, Id ((_, callee_name), TUnit)), args, unknown) ->

      let args = Array.of_list args in
      call_function callee_name args llbuilder

  (* Function call that return number (no gc for now) *)
  | p, Call ((pos, Id ((_, callee_name), TNumber)), args, unknown) ->

      let args = Array.of_list args in
      call_function callee_name args llbuilder

  (* Function call that return string, and result must be stored in intermediate gcroot variable *)
  | p, Call ((pos, Id ((_, callee_name), TString)), args, unknown) ->

      (* Create temporary variable to store string in *)
      let alloca = build_alloca (zend_string_ptr_type) "tmp" llbuilder in
      let tmp = build_bitcast alloca ptr_ptr_t "tmp2" llbuilder in
      let callee =
        match lookup_function "llvm.gcroot" llm with
          | Some callee -> callee
          | None ->
              raise (Llvm_error (sprintf "unknown function referenced: %s" "llvm.gcroot"))
      in
      let args2 = [|tmp; const_null i8_ptr_t|] in
      ignore (build_call callee args2 "" llbuilder);

      let args = Array.of_list args in
      let result = call_function callee_name args llbuilder in
      ignore(build_store result alloca llbuilder);
      result

  | expr -> raise (Llvm_not_implemented (sprintf "codegen_expr: %s" (show_expr expr)))

and is_numerical_op = function
  | Plus | Minus | Star | Slash | Starstar | Percent ->
      true
  | _ ->
      false

(**
 * Call a function and return LLVM IR call op
 *
 * @param name string
 * @param args Typedast.expr array
 * @return llvalue call
 *)
and call_function (name : string) (args : Typedast.expr array) llbuilder =
  (* Look up the name in the module table. *)
  let callee =
    match lookup_function name llm with
      | Some callee -> callee
      | None ->
          raise (Llvm_error (sprintf "unknown function referenced: %s" name))
  in
  let params = params callee in

  (* If argument mismatch error. *)
  if Array.length params == Array.length args then () else
    raise (Llvm_error "incorrect # arguments passed");

  let args = Array.map (fun arg -> codegen_expr arg llbuilder) args in
  build_call callee args "" llbuilder

let _ =
  (*
  let triple = "x86_64-unknown-linux-gnu" in
  let lltarget  = Llvm_target.Target.by_triple triple in
  let llmachine = Llvm_target.TargetMachine.create ~triple:triple lltarget in
  let lldly     = Llvm_target.TargetMachine.data_layout llmachine in
  set_target_triple (Llvm_target.TargetMachine.triple llmachine) llm;
  set_data_layout (Llvm_target.DataLayout.as_string lldly) llm;
  *)

  (*Llvm_X86.initialize ();*)

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

  (* Get command-line argument file *)
  let filename = if Array.length Sys.argv = 2 then
    Sys.argv.(1)
  else
    "test.php"
  in

  let open Parser_hack in
  SharedMem.(init default_config);
  let file_content = Utils.read_file filename in
  let parser_return = Parser_hack.program (Relative_path.Root, filename) file_content in

  (* If no error, dump a lot of info *)
  if parser_return.error = None then begin

    (*print_endline (Ast.show_program parser_return.ast);*)

    let program = Infer.infer_program 0 parser_return.ast in
    (*printf "%s\n" (Typedast.show_program program);*)

    (* Generate printd external function *)
    let f_param = {param_id = (Pos.none, "x"); param_type = TNumber} in
    let printd = {
      f_name = (Pos.none, "\\printd");
      f_params = [f_param];
      f_ret = TNumber;
      f_body = [];
    } in
    ignore (codegen_proto printd);

    (* Generate prints external function *)
    let f_param = {param_id = (Pos.none, "x"); param_type = TZend_string_ptr} in
    let prints = {
      f_name = (Pos.none, "\\prints");
      f_params = [f_param];
      f_ret = TNumber;
      f_body = [];
    } in
    ignore (codegen_proto prints);

    (* sqrt *)
    let f_param = {param_id = (Pos.none, "x"); param_type = TNumber} in
    let sqrt = {
      f_name = (Pos.none, "\\sqrt");
      f_params = [f_param];
      f_ret = TNumber;
      f_body = [];
    } in
    ignore (codegen_proto sqrt);

    (* Generate subsetphp_string_init external function *)
    let f_param1 = {param_id = (Pos.none, "str"); param_type = TString_literal} in
    let f_param2 = {param_id = (Pos.none, "len"); param_type = TInt} in
    let f_param3 = {param_id = (Pos.none, "persistent"); param_type = TInt} in
    let subsetphp_string_init = {
      f_name = (Pos.none, "\\subsetphp_string_init");
      f_params = [f_param1; f_param2; f_param3];
      f_ret = TZend_string_ptr;
      f_body = [];
    } in
    ignore (codegen_proto subsetphp_string_init);

    (* Generate subsetphp_concat_function *)
    let f_param1 = {param_id = (Pos.none, "value1"); param_type = TZend_string_ptr} in
    let f_param2 = {param_id = (Pos.none, "value2"); param_type = TZend_string_ptr} in
    let subsetphp_concat_function = {
      f_name = (Pos.none, "\\subsetphp_concat_function");
      f_params = [f_param1; f_param2];
      f_ret = TZend_string_ptr;
      f_body = [];
    } in
    ignore (codegen_proto subsetphp_concat_function);

    (* Generate llvm.gcroot *)
    let f_param1 = {param_id = (Pos.none, "x"); param_type = TPtr_ptr} in
    let f_param2 = {param_id = (Pos.none, "metadata"); param_type = TPtr} in
    let llvmgcroot = {
      f_name = (Pos.none, "\\llvm.gcroot");
      f_params = [f_param1; f_param2];
      f_ret = TUnit;
      f_body = [];
    } in
    ignore (codegen_proto llvmgcroot);

    (* Function for init gc *)
    let f_param1 = {param_id = (Pos.none, "heapsize"); param_type = TInt} in
    let subsetphp_gc_init = {
      f_name = (Pos.none, "\\llvm_gc_initialize");
      f_params = [f_param1];
      f_ret = TUnit;
      f_body = [];
    } in
    ignore (codegen_proto subsetphp_gc_init);

    (* llvm_gc_allocate *)
    (* TODO: rename to malloc so LLVM can recongnize its signature? *)
    let f_param1 = {param_id = (Pos.none, "size"); param_type = TInt64} in
    let llvm_gc_allocate = {
      f_name = (Pos.none, "\\llvm_gc_allocate"); 
      f_params = [f_param1];
      f_ret = TPtr;
      f_body = [];
    } in
    ignore (codegen_proto llvm_gc_allocate);

    (* initArray(array *a, initial size *)
    let f_param1 = {param_id = (Pos.none, "arr"); param_type = TLLVMArray} in
    let f_param2 = {param_id = (Pos.none, "initialSize"); param_type = TInt} in
    let infer_array = {
      f_name = (Pos.none, "\\inferArray"); 
      f_params = [f_param1; f_param2];
      f_ret = TPtr;
      f_body = [];
    } in
    ignore (codegen_proto infer_array);

    try
      ignore (codegen_program program);

      Llvm_analysis.assert_valid_module llm;
      (*dump_module llm;*)

      let filename_without_suffix = String.sub filename 0 (String.length filename - 4) in
      printf "writing file %s\n" (filename_without_suffix ^ ".bc");
      ignore (Llvm_bitwriter.write_bitcode_file llm (filename_without_suffix ^ ".bc"));

    with
      | Llvm_not_implemented msg ->
          begin
            print_endline "\nFATAL ERROR:";
            print_endline "Llvm_not_implemented:";
            print_endline (msg ^ "\n");
            exit 0
          end
    ;

  (* If parse error, print line and message etc *)
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
  end;
  ()

(*

  Example IR from hello program.

  @.str = private unnamed_addr constant [14 x i8] c"Hello, world!\00", align 1

  ; Function Attrs: nounwind uwtable
  define i32 @main() #0 {
    %1 = alloca i32, align 4
  %str = alloca i8*, align 8
  store i32 0, i32* %1
  store i8* getelementptr inbounds ([14 x i8]* @.str, i32 0, i32 0), i8** %str, align 8
  %2 = load i8** %str, align 8
  %3 = call i32 @puts(i8* %2)
  ret i32 0
}

my PHP test: $a = 'foo'

@foo = private unnamed_addr constant [4 x i8] c"foo\00"

define i32 @main() {
  entry:
    %"$a" = alloca i8
  store i8 0, i8* %"$a"
  store i8* getelementptr inbounds ([4 x i8]* @foo, i32 0, i32 0), i8* %"$a"
  ret i32 0
}
*)
