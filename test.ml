(**
 * Unit-tests for subsetphp
 *
 * @author Olle Harstedt
 * @since 2015-07-28

14:52:19 - ollehar: can I make ounit NOT test in parallell?
14:53:29 - companion_cube: use OUnit instead of OUnit2 
14:53:55 - ely-se: run your tests on a single-core machine
14:54:28 - ely-se: or a global mutex!
14:54:52 - zozozo: ollehar: do something like ./test -runner sequential

 *)

open OUnit2
open OUnitTest
open Parser_hack
open Printf
open Infer
open ListLabels
open Typedast

let test_simple_variable_inference test_ctxt =
  let code = "
    <?php
    $a = 10;
    $a = 'asd';
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  assert_raises
    (Infer.Error "cannot unify types Infer.TNumber and Infer.TString")
    (fun _ -> infer_program 0 parser_return.ast)

let test_variable_assignment text_ctxt =
  let code = "
    <?php
    $a = $b;
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  assert_raises
    (Failure "Can't use variable before it's defined: $b")
    (fun _ -> infer_program 0 parser_return.ast)

(** TODO: Test type tree
[(Typedast.Stmt
    Typedast.Expr (Typedast.TUnit,
      (<opaque>,
       Typedast.Binop ((Typedast.Eq None),
         (<opaque>, Typedast.Lvar ((<opaque>, "$a"), Typedast.TNumber)),
         (<opaque>, (Typedast.Int (<opaque>, "10"))), Typedast.TUnit))))]
*)
let test_variable_assignment2 test_ctxt =
  let code = "
    <?php
    $a = 10;
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  let ty = infer_program 0 parser_return.ast in
  let a_ty = match ty with
    | [(Stmt
        Expr (TUnit,
          (_,
           Binop ((Eq None),
             (_, Lvar ((_, "$a"), ty)),
             (_, (Int (_, "10"))), TUnit))))] ->
       ty
     | _ ->
         TUnknown
  in
  assert_equal ~msg:"Variable has type number"  a_ty TNumber

let test_variable_assignment3 test_ctxt =
  let code = "
    <?php
    $a = 10;
    $b = 'asd';
    $a = $b;
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  assert_raises
    (Infer.Error "cannot unify types Infer.TNumber and Infer.TString")
    (fun _ -> infer_program 0 parser_return.ast)

let test_function_return_type test_ctxt =
  let code = "
    <?php
    function foo() {
      return 10;
    }
    foo();
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  assert_raises
    (Infer.Error "cannot unify types Infer.TUnit and Infer.TNumber")
    (fun _ -> infer_program 0 parser_return.ast)

let test_function_return_type2 test_ctxt =
  let code = "
    <?php
    function foo() {
      return;
    }
    $a = foo();
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  assert_raises
    (Failure "Right-hand can't evaluate to void")
    (fun _ -> infer_program 0 parser_return.ast)

let test_function_return_type3 test_ctxt =
  let code = "
    <?php
    function foo() {
      return 10;
    }
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  let inferred_type = infer_program 0 parser_return.ast in
  let function_type = match inferred_type with
    | [(Fun {f_name = (_, "\\foo"); f_params = []; f_ret})] ->
        f_ret
    | _ -> 
        TUnknown
  in
  assert_equal ~msg:"Function returns number" function_type TNumber

let test_function_return_type4 test_ctxt =
  let code = "
    <?php
    function foo() {
      return 'asd';
    }
    $a = 10;
    $a = $a + foo();
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  assert_raises
    (Infer.Error "cannot unify types Infer.TString and Infer.TNumber")
    (fun _ -> infer_program 0 parser_return.ast)

let test_function_return_type5 test_ctxt =
  let code = "
    <?php
    function foo() {
      return 'asd';
    }
    function bar() {
      return 10;
    }
    $a = bar() + foo();
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  assert_raises
    (Infer.Error "cannot unify types Infer.TString and Infer.TNumber")
    (fun _ -> infer_program 0 parser_return.ast)

(** Infer argument type *)
let test_function_argument_type1 test_ctxt =
  let code = "
    <?php
    function foo($i) {
      return $i + 10;
    }
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  let inferred_type = infer_program 0 parser_return.ast in
  let arg_ty = match inferred_type with
    | [(Fun {
        f_name = (_, "\\foo");
        f_params = [{ param_id = (_, "$i");
                      param_type}];
        f_ret = TNumber })
      ] ->
        param_type
    | _ -> TUnknown
  in
  assert_equal ~msg:"Function argument is number" arg_ty TNumber

(** Infer argument type *)
let test_function_argument_type2 test_ctxt =
  let code = "
    <?php
    function foo($i, $j) {
      $i = 10;
      $j = 'asd';
    }
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  let inferred_type = infer_program 0 parser_return.ast in
  let arg_ty = match inferred_type with
    | [(Fun {
         f_name = (_, "\\foo");
         f_params = [{param_id = (_, "$i"); param_type = param_type_i};
                     {param_id = (_, "$j"); param_type = param_type_j}]; 
         f_ret})
      ] ->
        (param_type_i, param_type_j, f_ret)
    | _ ->
        (TUnknown, TUnknown, TUnknown)
  in
  assert_equal ~msg:"Function argument is number" arg_ty (TNumber, TString, TUnit)

let test_list = [
  "simple_variable_inference", test_simple_variable_inference;
  "variable_assignment", test_variable_assignment;
  "variable_assignment2", test_variable_assignment2;
  "variable_assignment3", test_variable_assignment3;
  "function_return_type", test_function_return_type;
  "function_return_type2", test_function_return_type2;
  "function_return_type3", test_function_return_type3;
  "function_return_type4", test_function_return_type4;
  "function_return_type5", test_function_return_type5;
  "function_argument_type1", test_function_argument_type1;
  "function_argument_type2", test_function_argument_type2;
]

let tear_down () test_ctxt =
  ()

let tag_test_suite =
  "tags" >::: (map test_list ~f:(fun (name, test_fn) ->
    name >:: (fun test_ctxt ->
      bracket ignore tear_down test_ctxt;
      test_fn test_ctxt
    )
  ))

let _ =
  SharedMem.(init default_config);
  run_test_tt_main tag_test_suite
