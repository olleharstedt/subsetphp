(**
 * Simple type-inference test for PHP
 *
 * This code stolen from blaha
 *
 * @since 2015-07-13
 *)

open List
open Ast

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
  Var of string
| Number (* We can't infer if number is int or float from pure PHP *)
| Unit  (* All statements return this, and functions without return *)
| Bool
| Arrow of typ * typ
[@@deriving show]

(*  A substitution is just a function typ -> typ.  *)

let identity (t : typ) = t

(*  replace (a, t) is the substitution that just replaces a with t.  *)

let rec replace at b = match at, b with
|  (a, t), Var b -> if a = b then t else Var b
|  (a, t), (Arrow (t1, t2)) ->
      Arrow (replace (a, t) t1, replace (a, t) t2)
|   (* No effect on other types. *)
    (a, t), t1 -> t1

(*  occurs : string * typ -> bool  *)

let rec occurs a b = match a, b with
| (a, Var b) -> (a = b)
| (a, Arrow(t1, t2)) -> occurs a t1 || occurs a t2
| (a, _) -> false

exception Circularity
exception Mismatch

(*  unify : typ * typ -> (typ -> typ)  *)
let rec unify (a : typ) t = match a, t with
| (Var a, t) ->
      if Var a = t then identity
      else if occurs a t then raise Circularity
      else replace (a, t) (* TODO Currying here? *)
| (t, Var a) -> unify (Var a) t
| (Number, Number) -> identity
| (Bool, Bool) -> identity
| (Unit, Unit) -> identity
| (Arrow(t1, t2), Arrow(t3, t4)) ->
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
let update e (x : string) (ty : typ) y = if x = y then ty else e y

(*  New type variable generator:  newtypevar () and reset ()  *)
let letters = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m"; "n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]
let cnt = ref 0
let typevars = ref letters

let newtypevar () =
  if length (! typevars) = 0 then (
    cnt := ! cnt + 1;
    typevars := map (fun s -> s ^ string_of_int (! cnt)) letters
  ) else ();
  let tmp = Var (hd (! typevars)) in
  (typevars := tl (! typevars));
  tmp

let reset () = (
  cnt := 0;
  typevars := letters
)

let rec w_stmt (env : env) (term : stmt) = match env, term with
| env, Expr (_, expr) ->
    let (s1, t1) = w_expr_ env expr in
    (s1 @@ env, Unit)
| _ -> failwith "Not implemented: stmt"

and w_def (env : env) (term : def) = match env, term with
| env, Stmt stmt ->
    let (s1, t1) = w_stmt env stmt in
    let s2 = unify t1 Unit in
    (s2 @@ s1, Unit)
| _ -> failwith "Not implemented: def"

(**
 * Milner's algorithm W : (string -> typ) * term -> (typ -> typ) * typ
 *
 * @param env string -> typ
 * @param term Ast
 *)
and w_expr_ (env : env) (term : expr_) = match env, term with
| env, Lvar (_, x) ->
    (identity, env x)
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
    (identity, Number)
| (env, _) -> failwith "Not implemented: expr_"

(**  
 * Put a type in canonical form, such that the type variables start
 * from scratch.
 *)
let rec canonical' env t = match env, t with
| (env, Var v) ->
    begin
      try env, (env v) with UnboundID ->
        let t = newtypevar() in
        (update env v t), t
    end
| (env, Arrow (t1, t2)) ->
      let (env, t1') = canonical' env t1 in
      let (env, t2') = canonical' env t2 in
        (env, Arrow (t1', t2'))
| (env, x) -> (env, x)

let canonical t =
  reset ();
  let (_, t) = canonical' emptyenv t in
  t

(**  
 * Here's a driver program that uses W to find the principal type of e
 * and then displays it nicely.
 *)
let infer exp =
  let (s, t) = (reset (); w_def emptyenv exp) in
  let typ = canonical t in
  print_endline ("The principal type is " ^ show_typ typ)

let _ =
  infer 
    (Ast.Stmt
        (Ast.Expr
           (Pos.none,
            Ast.Binop ((Ast.Eq None), (Pos.none, (Ast.Lvar (Pos.none, "$a"))),
              (Pos.none, (Ast.Int (Pos.none, "10")))))))
