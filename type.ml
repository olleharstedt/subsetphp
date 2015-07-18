(**
 * Simple type-inference test for PHP
 *
 * This code stolen from blaha
 *
 * @since 2015-07-13
 *)

open List
open Ast
open Parser_hack

type term =
  AST_ID of string
| AST_NUM of int
| AST_BOOL of bool
| AST_FUN of (string * term)
| AST_APP of (term * term)
| AST_SUCC
| AST_PRED
| AST_ISZERO
| AST_IF of (term * term * term)
| AST_REC of (string * term)
| AST_ERROR of string

(* Infix function composition *)
let (@@) f g x = f (g x)

type typ =
  TVar of string
| TNumber (* We can't infer if number is int or float from pure PHP *)
| TUnit  (* All statements return this, and functions without return *)
| TString
| TBool
| TArrow of typ (*list*) * typ  (* Functions with more than one argument are represented as Arraw(Tuple -> typ) *)
[@@deriving show]

(*  A substitution is just a function typ -> typ.  *)

let identity (t : typ) = t

(*  replace (a, t) is the substitution that just replaces a with t.  *)

let rec replace at b = match at, b with
|  (a, t), TVar b -> if a = b then t else TVar b
|  (a, t), (TArrow (t1, t2)) ->
      TArrow (replace (a, t) t1, replace (a, t) t2)
|   (* No effect on other types. *)
    (a, t), t1 -> t1

(*  occurs : string * typ -> bool  *)

let rec occurs a b = match a, b with
| (a, TVar b) -> (a = b)
| (a, TArrow(t1, t2)) -> occurs a t1 || occurs a t2
| (a, _) -> false

exception Circularity
exception Mismatch

(*  unify : typ * typ -> (typ -> typ)  *)
let rec unify (a : typ) t = match a, t with
| (TVar a, t) ->
      if TVar a = t then identity
      else if occurs a t then raise Circularity
      else replace (a, t) (* TODO Currying here? *)
| (t, TVar a) -> unify (TVar a) t
| (TNumber, TNumber) -> identity
| (TBool, TBool) -> identity
| (TUnit, TUnit) -> identity
| (TString, TString) -> identity
| (TArrow(t1, t2), TArrow(t3, t4)) ->
      let s1 = unify t1 t3 in
      let s2 = unify (s1 t2) (s1 t4) in
      s2 @@ s1
| (a, b) ->
    print_endline (show_typ a);
    print_endline (show_typ b);
    raise Mismatch

(*  An environment is a function string -> typ.
 *  I've included code for the empty environment
 *  and code that produces the updated environment E[x : t].
 *)

exception UnboundID

type env = string -> typ

let emptyenv (x : string) : typ = raise UnboundID

(*  update : (string -> typ) -> string -> typ -> string -> typ  *)
let update env (x : string) (ty : typ) y =
  if x = y then ty else env y

(*  New type variable generator:  newtypevar () and reset ()  *)
let letters = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m"; "n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]
let cnt = ref 0
let typevars = ref letters

let newtypevar () =
  if length (! typevars) = 0 then (
    cnt := ! cnt + 1;
    typevars := map (fun s -> s ^ string_of_int (! cnt)) letters
  ) else ();
  let tmp = TVar (hd (! typevars)) in
  (typevars := tl (! typevars));
  tmp

let reset () = (
  cnt := 0;
  typevars := letters
)

let rec w_program (env : env) (term : def list) =
  match env, term with
  | env, [] -> env
  | env, def::[] ->
      let (s1, t1) = w_def env def in
      let s2 = unify t1 TUnit in
      s2 @@ s1

and w_def (env : env) (term : def) = match env, term with
| env, Stmt stmt ->
    let (s1, t1) = w_stmt env stmt in
    let s2 = unify t1 TUnit in
    (s2 @@ s1, TUnit)
| _ -> failwith "Not implemented: def"

and w_stmt (env : env) (term : stmt) = match env, term with
| env, Expr (_, expr) ->
    let (s1, t1) = w_expr_ env expr in
    (s1 @@ env, TUnit)
| _ -> failwith "Not implemented: stmt"

(**
 * Milner's algorithm W : (string -> typ) * term -> (typ -> typ) * typ
 *
 * @param env string -> typ
 * @param term Ast
 *)
and w_expr_ (env : env) (term : expr_) =
  match env, term with
  | env, Lvar (_, x) ->
      begin
        (* TODO: Must update env here with new type variable *)
        let (env, _) = try (env, env x) with UnboundID ->
          let t = newtypevar() in
          update env x t, env x
        in
        (identity, env x)
      end
  | (env, Binop (op, (_, exp1), (_, exp2))) ->
      (
        match op with
        | Eq None ->
          let (s1, t1) = w_expr_ env exp2 in
          let (s2, t2) = w_expr_ (s1 @@ env) exp2 in
          let s = s1 @@ s2 in
          (s, s t2)
        | _ -> failwith "Not implemented: op"
      )
  | (env, Int (_, pstr)) ->
      (identity, TNumber)
  | (env, String (_, str)) ->
      (identity, TString)
  | (env, _) -> failwith "Not implemented: expr_"

(**
 * Put a type in canonical form, such that the type variables start
 * from scratch.
 *)
let rec canonical' env t = match env, t with
| (env, TVar v) ->
    begin
      try env, (env v) with UnboundID ->
        let t = newtypevar() in
        (update env v t), t
    end
| (env, TArrow (t1, t2)) ->
      let (env, t1') = canonical' env t1 in
      let (env, t2') = canonical' env t2 in
        (env, TArrow (t1', t2'))
| (env, x) -> (env, x)

let canonical t =
  reset ();
  let (_, t) = canonical' emptyenv t in
  t

(**
 * Here's a driver program that uses W to find the principal type of e
 * and then displays it nicely.
 *)
let infer (prog : program) =
  let envref = ref emptyenv in
  reset ();
  for i = 0 to length prog - 1 do
    let def = nth prog i in
    let (s, t) = (w_def (!envref) def) in
    let typ = canonical t in
    print_endline ("The principal type is " ^ show_typ typ);
    envref := s;
  done
  (*let (s, t) = (w_def emptyenv exp) in*)

  (*
   * for each def
   *   w_def emptyenv
   *)

(**
 * Read file, return string, no escape
 *
 * @param filename string
 * @return string
 *)
let read_file filename =
  let in_channel = open_in filename in
  let file_content = ref "" in
  (try while true do begin
    let line = input_line in_channel in
    file_content := !file_content ^ line
  end done
  with End_of_file -> close_in in_channel);
  (*eprintf "file_content = %s" !file_content;*)
  !file_content

let _ =
  SharedMem.(init default_config);
  let file_content = read_file "test.php" in
  let parser_return = program (Relative_path.Root, "") file_content in
  print_endline (Ast.show_program parser_return.ast);

  infer parser_return.ast
