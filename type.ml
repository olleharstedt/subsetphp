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

type typ = Var of string | Int | Bool | Arrow of typ * typ
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
| (Int, Int) -> identity
| (Bool, Bool) -> identity
| (Arrow(t1, t2), Arrow(t3, t4)) ->
      let s1 = unify t1 t3 in
      let s2 = unify (s1 t2) (s1 t4) in
      s2 @@ s1
| (_, _) -> raise Mismatch

(*  An environment is a function string -> typ.
 *  I've included code for the empty environment
 *  and code that produces the updated environment E[x : t].
 *)

exception UnboundID

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
(*  Milner's algorithm W : (string -> typ) * term -> (typ -> typ) * typ *)

let rec w env t = match env, t with
| (env, AST_ID x) ->
      (* E(x) = t
         ----------
         E |- x : t *)
      (identity, env x)

| (env, AST_NUM _)   ->
      (* E |- n : int *)
      (identity, Int)

| (env, AST_BOOL _) ->
      (* E |- true : bool
         E |- false : bool *)
      (identity, Bool)

| (env, AST_SUCC) ->
      (* E |- succ : int -> int *)
      (identity, Arrow (Int, Int))

| (env, AST_PRED) ->
      (* E |- pred : int -> int *)
      (identity, Arrow (Int, Int))

| (env, AST_ISZERO) ->
      (* E |- iszero : int -> bool *)
      (identity, Arrow (Int, Bool))

| (env, AST_IF (e1, e2, e3)) ->
      (* E |- e1 : bool  E |- e2 : t  E |- e3 : t
         ----------------------------------------
         E |- if e1 then e2 else e3 : t           *)
      let (s1, t1) = w env e1 in
      let s2 = unify t1 Bool in
      let (s3, t2) = w (s2 @@ s1 @@ env) e2 in
      let (s4, t3) = w (s3 @@ s2 @@ s1 @@ env) e3 in
      let s5 = unify (s4 t2) t3 in
        (s5 @@ s4 @@ s3 @@ s2 @@ s1, s5 t3)

| (env, AST_FUN (x, e)) ->
      (* E[x : t1] |- e : t2
         -------------------------
         E |- fn x => e : t1 -> t2 *)
      let t1 = newtypevar() in
      let (s, t2) = w (update env x t1) e in
        (s, s (Arrow (t1, t2)))

| (env, AST_APP (e1, e2)) ->
      (* E |- e1 : t1 -> t2  E |- e2 : t1
         --------------------------------
         E |- e1 e2 : t2                  *)
      let (s1, t1) = w env e2 in
      let t2 = newtypevar() in
      let (s2, t3) = w (s1 @@ env) e1 in
      let s3 = unify (s2 t3) (Arrow ((s2 @@ s1) t1, t2)) in
      let s = s3 @@ s2 @@ s1 in
        (s, s t2)

| (env, AST_REC (x, e)) ->
      (* E[x : t] |- e : t
         -------------------
         E |- rec x => e : t *)
      let t = newtypevar() in
      let (s1, te) = w (update env x t) e in
      let s2 = unify (s1 t) (s1 te) in
      let s = s2 @@ s1 in
        (s, s t)

| (env, AST_ERROR _) -> raise Mismatch (* shouldn't happen *)

(*  Put a type in canonical form, such that the type variables start
 *  from scratch.
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
  (reset ();
  let (_, t) = canonical' emptyenv t
  in
    t
  )

(*  Here's a driver program that uses W to find the principal type of e
 *  and then displays it nicely.
 *)

let infer e =
  let (s, t) = (reset (); w emptyenv e)
  in
    print_endline ("The principal type is " ^ show_typ (canonical t) ^ "\n")

let _ =
  infer (AST_NUM 10)

(*
(* test for // wrongly independent substitutions in AST_APP *)
infer (parsestr "fn f => fn g => g (f true) (f 1)");
exception Mismatch

(* test for wrong ordering of substitutions in AST_REC *)
infer (parsestr "rec f => rec g => fn x => f (g x)");
'a -> 'a

infer (parsestr "fn f => fn x => f (f x)");
('a -> 'a) -> 'a -> 'a

infer (parsestr "fn f => fn g => fn x => f (g x)");
('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

infer (parsestr "fn b => if b then 1 else 0");
bool -> int

infer (parsestr "rec f => fn b => if b then 1 else f true");
bool -> int

infer (parsestr "rec f => fn x => f x");
'a -> 'b

infer (parsestr "rec f => rec g => fn x => f (f x)");
'a -> 'a

infer (parsestr "rec m => fn x => fn y => if iszero y then x else m (pred x) (pred y)");
int -> int -> int

infer (parsestr "rec even => fn n => if iszero n then true else if iszero (pred n) then false else even (pred (pred n))");
int -> bool

infer (parsefile "twice.pcf");
exception Circularity

infer (parsefile "minus.pcf");
int

infer (parsefile "factorial.pcf");
int

infer (parsefile "fibonacci.pcf");
int

infer (parsefile "lists.pcf");
exception Mismatch

infer (parsefile "not.pcf");
exception Mismatch
*)
