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
let i8_t = i8_type llctx
let i8_ptr_t = pointer_type i8_t
let ptr_t = pointer_type i8_t
let zend_string_type = named_struct_type llctx "zend_string"
let zend_string_ptr_type = pointer_type zend_string_type
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10
let zero = const_int i32_t 0

(**
 * Return LLVM type of typed AST type
 *
 * @param Typedast.ty ty
 * @return lltype
 *)
let llvm_ty_of_ty ty = match ty with
  | TNumber -> double_type
  | TString -> i8_ptr_t  (* Pointer to const char* *)
  | TZend_string_ptr -> zend_string_ptr_type
  | _ -> raise (Llvm_not_implemented (sprintf "llvm_ty_of_ty: %s" (show_ty ty)))

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
        llvm_ty_of_ty arg_type
      ) args in
      let ft = function_type (llvm_ty_of_ty f_ret) llvm_args in
      let f = match lookup_function name llm with
        | None -> 
            let name = String.sub name 1 (String.length name - 1) in  (* Strip leading \ (namespace thing) *)
            declare_function name ft llm

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
        Hashtbl.add named_values n a;
        ) (params f);
      f
    end


(* Create an alloca for each argument and register the argument in the symbol
 * table so that references to it will succeed. *)
let create_argument_allocas the_function fun_ llbuilder =
  let args = List.map (fun param -> match param with {param_id = (pos, var_name); param_type} -> var_name) fun_.f_params in
  let args = Array.of_list args in
  Array.iteri (fun i ai -> 
    let var_name = args.(i) in
    (* Create an alloca for this variable. *)
    let alloca = create_entry_block_alloca the_function var_name double_type in

    (* Store the initial value into the alloca. *)
    ignore(build_store ai alloca llbuilder);

    (* Add arguments to variable symbol table. *)
    Hashtbl.add named_values var_name alloca;
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
  
  (* TODO: This means all function must come before "main" script code? *)
  Hashtbl.clear named_values;

  let the_function = codegen_proto fun_ in
  let llbuilder = builder_at_end llctx (entry_block the_function) in

  (* If this is an operator, install it. *)
  (*
  begin match proto with
  | Ast.BinOpPrototype (name, args, prec) ->
  let op = name.[String.length name - 1] in
  Hashtbl.add Parser.binop_precedence op prec;
  | _ -> ()
  end;
  *)

  (* Create a new basic block to start insertion into. *)
  let bb = append_block llctx "entry" the_function in
  position_at_end bb llbuilder;

  try
    (* Add all arguments to the symbol table and create their allocas. *)
    create_argument_allocas the_function fun_ llbuilder;

    ignore (codegen_block fun_.f_body llbuilder);

    (* Validate the generated code, checking for consistency. *)
    Llvm_analysis.assert_valid_function the_function;

    (* Optimize the function. *)
    let _ = PassManager.run_function the_function the_fpm in

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
  ()

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

  | For ( (p1, Binop ((Eq None), (p2, Lvar ((p3, var_name), TNumber)), (p4, (Int (p5, var_value))), TUnit)) as start, end_, step, body) ->
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
      try Some (Hashtbl.find named_values var_name) with Not_found -> None
      in
      Hashtbl.add named_values var_name alloca;

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
        | Some old_val -> Hashtbl.add named_values var_name old_val
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
      let variable = try Hashtbl.find named_values lvar_name with
        | Not_found ->
            raise (Llvm_error (sprintf "Lvar is used on rhs before ever used on the lhs: %s" lvar_name))
      in
      build_load variable lvar_name llbuilder
  | p, Number nr ->
      const_float double_type nr;
  | p, String (pos, str) ->
      (*let str_val = const_string llctx str in*)
      (*define_global str str_val llm *)
      build_global_stringptr str str llbuilder

      (* Init string with zend_string_init *)
      (* Make string interend *)
      (* Return pointer to string *)
  | p, Int (pos, i) ->
      let f = float_of_string i in
      const_float double_type f;

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
      let variable = try Hashtbl.find named_values lvar_name with
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
      let variable = try Hashtbl.find named_values lvar_name with
        | Not_found ->
            (* Should not happen *)
            failwith "Tried to use -= on variable that is not defined"
        in
      let rexpr_code = codegen_expr rexpr llbuilder in
      let load_code = build_load variable lvar_name llbuilder in
      let add_code = build_fsub load_code rexpr_code "addtmp" llbuilder in

      ignore (build_store add_code variable llbuilder);
      add_code

  (* Assign number to variable *)
  | p, Binop (Eq None, (lhs_pos, Lvar ((lvar_pos, lvar_name), TNumber)), value_expr, binop_ty) ->
      let the_function = block_parent (insertion_block llbuilder) in
      let variable = try Hashtbl.find named_values lvar_name with
        | Not_found ->
            (* If variable is not found in this scope, create a new one *)
            let alloca = create_entry_block_alloca the_function lvar_name double_type in
            let init_val =  const_float double_type 0.0 in
            ignore (build_store init_val alloca llbuilder);
            Hashtbl.add named_values lvar_name alloca;
            alloca
            in
      let value_expr_code = codegen_expr value_expr llbuilder in
      ignore (build_store value_expr_code variable llbuilder);
      value_expr_code

  (* Assign string to variable *)
  | p, Binop (Eq None, (lhs_pos, Lvar ((lvar_pos, lvar_name), TString)), value_expr, binop_ty) ->
      let the_function = block_parent (insertion_block llbuilder) in
      let variable = try Hashtbl.find named_values lvar_name with
        | Not_found ->
            (* If variable is not found in this scope, create a new one *)
            let builder = builder_at llctx (instr_begin (entry_block the_function)) in
            let alloca = build_alloca i8_ptr_t lvar_name builder in
            (*
            let init_val =  const_int i8_t 0 in
            ignore (build_store init_val alloca llbuilder);
            *)
            Hashtbl.add named_values lvar_name alloca;
            alloca
      in
      let value_expr_code = codegen_expr value_expr llbuilder in
      (*
      print_endline (string_of_lltype (type_of value_expr_code));
      print_endline (string_of_lltype (type_of value_expr_code));
      *)
      (* GEP = get element pointer *)
      (*let ptr = build_in_bounds_gep value_expr_code [|zero|] "" llbuilder in*)
      ignore (build_store value_expr_code variable llbuilder);
      value_expr_code

  (* Numerical operations *)
  | p, Binop (bop, expr1, expr2, binop_ty) when is_numerical_op bop ->
      let lhs = codegen_expr expr1 llbuilder in
      let rhs = codegen_expr expr2 llbuilder in
      begin match bop with 
        | Plus ->
            build_fadd lhs rhs "addtmp" llbuilder
        | Minus ->
            build_fsub lhs rhs "subtmp" llbuilder
        | bop -> raise (Llvm_not_implemented (show_bop bop))
      end;

  (* . concatenation *)
      (*
  | p, Typedast.Binop (Typedast.Dot, expr1, expr2, TString) ->
  (*| p, Typedast.Binop (Typedast.Dot, (p, Typedast.Lvar ((p, "$a"), Typedast.TString)), (p, (Typedast.String (p, "qwe"))), Typedast.TString) ->*)
      let lhs = codegen_expr expr1 llbuilder in
      let rhs = codegen_expr expr2 llbuilder in
      (* Create new string, zend_string_init *)
      (* Contcat lhs and rhs using concat_function *)
      (* Interned strings and dynamic strings should return same pointer type *)
      ()
*)

  (* Function call *)
  | p, Call ((pos, Id ((_, callee_name), call_ty)), args, unknown) ->
  (* (p, Call ((pos, Id ((_, name),  .TUnit)),  [(<opaque>, Typedast.Lvar ((<opaque>, \"$i\"), Typedast.TNumber))], [\n   ]))") *)

      let args = Array.of_list args in
      call_function callee_name args llbuilder

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
 * @param args array
 * @return llvalue call
 *)
and call_function name args llbuilder = 
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
  build_call callee args name llbuilder

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
  let parser_return = Parser_hack.program (Relative_path.Root, "test.php") file_content in

  (* If no error, dump a lot of info *)
  if parser_return.error = None then begin

    print_endline (Ast.show_program parser_return.ast);

    let program = Infer.infer_program 0 parser_return.ast in
    printf "%s\n" (Typedast.show_program program);

    (* Generate printd external function *)
    let f_param = {param_id = (Pos.none, "x"); param_type = TNumber} in
    let printd = {
      f_name = (Pos.none, "\\printd"); 
      f_params = [f_param];
      f_ret = TNumber;
      f_body = [];
    } in
    ignore (codegen_proto printd);

    (* Generate zend_string_init external function *)
    let f_param1 = {param_id = (Pos.none, "str"); param_type = TString} in
    let f_param2 = {param_id = (Pos.none, "len"); param_type = TNumber} in
    let f_param3 = {param_id = (Pos.none, "persistent"); param_type = TNumber} in
    let zend_string_init = {
      f_name = (Pos.none, "\\zend_string_init"); 
      f_params = [f_param1; f_param2; f_param3];
      f_ret = TZend_string_ptr;
      f_body = [];
    } in
    ignore (codegen_proto zend_string_init);

    ignore (codegen_program program);

    dump_module llm;

    Llvm_analysis.assert_valid_module llm;

    ignore (Llvm_bitwriter.write_bitcode_file llm "llvm_test.bc")

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
