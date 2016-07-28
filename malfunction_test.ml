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

(*
21:07:14 - mrvn: a ref is just a block with 1 value.
21:07:42 - ollehar: with value 1, or with one value?
21:07:52 - mrvn: one
21:07:56 - ollehar: ok, thanks
21:08:48 - mrvn: ref 1 is a block containing 3 (1 << 1 + tag), ref 1.0 is a block containing a pointer to the float block.
21:09:10 - mrvn: unless thats optimized like float array
21:09:30 - ollehar: what did that 3 come from?
21:09:42 - mrvn: Val_int(1)
21:10:11 - ollehar: ok
22:06:51 - mrvn: # type 'a ref = { mutable contents: 'a };;
 *)

(*
 * `autocmd WinLeave * if exists('b:terminal_job_id') | let g:last_term_id = b:terminal_job_id`, and `autocmd BufWrite <buffer> call jobsend(b:terminal_job_id, "make\n")`
 *)
(*:autocmd! BufWritePost * call jobsend(1, "clear; make malf; ./malf\n") *)

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
    (dum, Atom "module");
    (dum, List [
      (dum, Atom "_");
      (dum, List [
          (dum, Atom "apply");
          (dum, List [
              (dum, Atom "global");
              (dum, Var "Pervasives");
              (dum, Var "print_string")
          ]);
          (dum, String "Hello, world!\n");
      ])
    ]);
    (dum, List [
      (dum, Atom "export");
    ])
  ]))

(*
$myVar = 10;
echo 10;
 *)
let basic_code =
  (dum, List ([
    (dum, Atom "module");
    (dum, List [
      (dum, Atom "_");
      (dum, List [
        (dum, Atom "let");
        (dum, List [
          (dum, Var "asd");
          (dum, Int 10);
        ]);
        (dum, List [
          (dum, Atom "apply");
          (dum, List [
              (dum, Atom "global");
              (dum, Var "Pervasives");
              (dum, Var "print_int")
          ]);
          (dum, Var "asd")
        ]);
      ]);
    ]);
    (dum, List [
      (dum, Atom "export");
    ])
  ]))

(**
 * $a = 10
 * $a = 5
 * echo $a

(module
  (_
    (let ($a (block (tag 0) 10)) ($a (block (tag 0) 5))
      (apply (global $Pervasives $print_int) (field 0 $a))))
  (export))

 *)
let ref_var_code = 
  (dum, List ([
    (dum, Atom "module");
    (dum, List [
      (dum, Atom "_");
      (dum, List [
        (dum, Atom "let");
        (dum, List [
          (dum, Var "a");
          (dum, List [
            (dum, Atom "block");
            (dum, List [
              (dum, Atom "tag");
              (dum, Int 0)
            ]);
            (dum, Int 10)
          ]);
        ]);
        (dum, List [
          (dum, Var "a");
          (dum, List [
            (dum, Atom "block");
            (dum, List [
              (dum, Atom "tag");
              (dum, Int 0)
            ]);
            (dum, Int 5)
          ]);
        ]);
        (dum, List [
          (dum, Atom "apply");
          (dum, List [
              (dum, Atom "global");
              (dum, Var "Pervasives");
              (dum, Var "print_int")
          ]);
          (dum, List [
            (dum, Atom "field");
            (dum, Int 0);
            (dum, Var "a");
          ]);
        ]);
      ]);
    ]);
    (dum, List [
      (dum, Atom "export");
    ])
  ]))

(*
 * $a = 1.0
 * echo $a
 *)
let double_code =
  (dum, List ([
    (dum, Atom "module");
    (dum, List [
      (dum, Atom "_");
      (dum, List [
        (dum, Atom "let");
        (dum, List [
          (dum, Var "a");
          (dum, List [
            (dum, Atom "block");
            (dum, List [
              (dum, Atom "tag");
              (dum, Int 0)
            ]);
            (dum, List [
              (dum, Atom "block");
              (dum, List [
                (dum, Atom "tag");
                (dum, Int Obj.double_tag)
              ]);
              (dum, String "1.0")
            ]);
          ]);
        ]);
        (dum, List [
          (dum, Atom "apply");
          (dum, List [
              (dum, Atom "global");
              (dum, Var "Pervasives");
              (dum, Var "print_float")
          ]);
          (dum, List [
            (dum, Atom "field");
            (dum, Int 0);
            (dum, Var "a");
          ]);
        ]);
      ]);
    ]);
    (dum, List [
      (dum, Atom "export");
    ])
  ]))

let _ =
  (*
  let options = [`Shared] in
  let lm = Malfunction_compiler.module_to_lambda ~options test_code in
  *)

  Malfunction_sexp.print Format.str_formatter double_code;
  let str = Format.flush_str_formatter () in
  print_endline str;
  let e = Malfunction_parser.parse_mod double_code in
  let l = Malfunction_compiler.module_to_lambda e in
  let c = Malfunction_compiler.lambda_to_cmx "malf_file" "malf_output" l in

  ()
