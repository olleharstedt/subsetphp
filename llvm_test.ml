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
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10
let zero = const_int i32_t 0

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
 * @param def def
 * @return llvalue
 *)
let codegen_proto def = 
  match def with
  | Fun {f_name = (f_name_pos, name); f_params; f_ret} -> begin
      (* Make the function type: double(double,double) etc. *)
      let args = List.map (fun param -> match param with {param_id; param_type} -> param_type) f_params in
      let args = Array.of_list args in
      let doubles = Array.make (Array.length args) double_type in
      let ft = function_type double_type doubles in
      let f = match lookup_function name llm with
        | None -> declare_function name ft llm

        (* If 'f' conflicted, there was already something named 'name'. If it
         * has a body, don't allow redefinition or reextern. *)
        | Some f ->
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
  | _ ->
    failwith "codegen_proto: Only Fun fun_ allowed"


(**
 * Generate LLVM IR for program
 *
 * All "global" PHP variables are wrapped in the main function.
 *
 * @param Typedast.program program
 * @return ?
 *)
let rec codegen_program program =

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
    | Fun fun_ :: tail ->
        ()
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
and codegen_stmt stmt llbuilder : llvalue = 
  match stmt with
  | Block block ->
      codegen_block block llbuilder
  | Expr (ty, expr) ->
      codegen_expr expr llbuilder
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

  | _ -> raise (Llvm_not_implemented (sprintf "codegen_stmt: %s" (show_stmt stmt)))

  (**
   * Generate LLVM IR for expr
 *
 * @param expr Typedast.expr
 * @param llbuilder
 * @return llvalue
 *)
and codegen_expr expr llbuilder : llvalue = 
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
        | p, Int (pos, i) ->
            print_endline "";
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
  | p, Call ((pos, Id ((_, callee), call_ty)), args, unknown) ->
  (* (p, Call ((pos, Id ((_, name),  .TUnit)),  [(<opaque>, Typedast.Lvar ((<opaque>, \"$i\"), Typedast.TNumber))], [\n   ]))") *)

     (* Look up the name in the module table. *)
      let callee =
        match lookup_function callee llm with
          | Some callee -> callee
          | None -> raise (Llvm_error "unknown function referenced")
      in
      let params = params callee in

      (* If argument mismatch error. *)
      let args = Array.of_list args in
      if Array.length params == Array.length args then () else
        raise (Llvm_error "incorrect # arguments passed");

      let args = Array.map (fun arg -> codegen_expr arg llbuilder) args in
      build_call callee args "calltmp" llbuilder

  | expr -> raise (Llvm_not_implemented (sprintf "codegen_expr: %s" (show_expr expr)))

and is_numerical_op = function
  | Plus | Minus | Star | Slash | Starstar | Percent ->
      true
  | _ ->
      false

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
    let printd = Fun {
      f_name = (Pos.none, "printd"); 
      f_params = [f_param];
      f_ret = TNumber
    } in
    ignore (codegen_proto printd);

    ignore (codegen_program program);

    dump_module llm;

    Llvm_analysis.assert_valid_module llm;

    let _ = Llvm_bitwriter.write_bitcode_file llm "llvm_test.bc" in
    ()
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
