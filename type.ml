(**
 * Simple type-inference test for PHP
 *
 * This code stolen from blaha
 *
 * @since 2015-07-13
 *)

open List

(* Infix function composition *)
let (@@) f g x = f (g x)

type typ = VAR of string | INT | BOOL | ARROW of typ * typ
(*[@@ deriving show]*)

(*  A substitution is just a function typ -> typ.  *)

let identity (t : typ) = t

(*  replace (a, t) is the substitution that just replaces a with t.  *)

let rec replace at b = match at, b with
|  (a, t), VAR b -> if a = b then t else VAR b
|  (a, t), (ARROW (t1, t2)) ->
      ARROW (replace (a, t) t1, replace (a, t) t2)
|   (* No effect on other types. *)
    (a, t), t1 -> t1

(*  occurs : string * typ -> bool  *)

let rec occurs a b = match a, b with
| (a, VAR b) -> (a = b)
| (a, ARROW(t1, t2)) -> occurs a t1 || occurs a t2
| (a, _) -> false

exception Circularity
exception Mismatch

(*  unify : typ * typ -> (typ -> typ)  *)

let rec unify (a : typ) t = match a, t with
| (VAR a, t) ->
      if VAR a = t then identity
      else if occurs a t then raise Circularity
      else replace (a, t) (* TODO Currying here? *)
| (t, VAR a) -> unify (VAR a) t
| (INT, INT) -> identity
| (BOOL, BOOL) -> identity
| (ARROW(t1, t2), ARROW(t3, t4)) ->
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

let _ =
  let letters = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m"; "n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"] in
  let cnt = ref 0 in
  let typevars = ref letters in
  let newtypevar () =
    if length (! typevars) = 0 then (
      cnt := ! cnt + 1;
      typevars := map (fun s -> s ^ string_of_int (! cnt)) letters
    ) else ();
    let tmp = VAR (hd (! typevars)) in 
    (typevars := tl (! typevars));
    tmp
  in

  let reset () = (
    cnt := 0;
    typevars := letters
  )
  in ()
