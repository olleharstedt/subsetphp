(**
 * Unit-tests for subsetphp
 *
 * @author Olle Harstedt
 * @since 2015-07-28
 *)

open OUnit2
open OUnitTest
open Parser_hack
open Printf
open Infer
open ListLabels

let test_simple_variable_inference test_ctxt =
  let code = "
    <?php
    $a = 10;
    $a = 'asd';
  " in
  let parser_return = Parser_hack.program (Relative_path.Root, "") code in
  assert_raises 
    (Infer.Error "cannot unify types Infer.TNum and Infer.TString")
    (fun _ -> infer_program Env.empty 0 parser_return.ast)

let test_list = [
  "simple_variable_inference", test_simple_variable_inference;
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
