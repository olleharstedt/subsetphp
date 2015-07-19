(**
 * Code taken from: https://github.com/tomprimozic/type-systems
 *)

open Ast

type name = string
[@@deriving show]

type expr =
  | Var of name                           (* variable *)
  | Call of expr * expr list              (* application *)
  | Fun of name list * expr               (* abstraction *)
  (*| Let of name * expr * expr           (* let *)*)
  | Let of name * expr                    (* PHP variables don't have scope *)
  | Num of int                            (* PHP can't infer int or float, use num *)
  | String of string

(** PHP programs are lists of expressions/statements *)
type progrem = expr list

type id = int
[@@deriving show]
type level = int
[@@deriving show]

type ty =
  | TConst of name                    (* type constant: `int` or `bool` *)
  | TApp of ty * ty list              (* type application: `list[int]` *)
  | TArrow of ty list * ty            (* function type: `(int, int) -> int` *)
  | TVar of tvar ref                  (* type variable *)
  | TNum
  | TString
  | TUnit
[@@deriving show]

and tvar =
  | Unbound of id * level
  | Link of ty
  | Generic of id
[@@deriving show]

let current_id = ref 0

let next_id () =
  let id = !current_id in
  current_id := id + 1 ;
  id

let reset_id () = current_id := 0


let new_var level = TVar (ref (Unbound(next_id (), level)))
let new_gen_var () = TVar (ref (Generic(next_id ())))


exception Error of string
let error msg = raise (Error msg)


module Env = struct
  module StringMap = Map.Make (String)
  type env = ty StringMap.t

  let empty : env = StringMap.empty
  let extend env name ty = StringMap.add name ty env
  let lookup env name = StringMap.find name env
  let dump env =
    StringMap.iter (fun key value ->
      print_endline (Printf.sprintf "key = %s, value = %s" key (show_ty value))
    ) env;
    print_endline "enddump"
end


let occurs_check_adjust_levels tvar_id tvar_level ty =
  let rec f = function
    | TVar {contents = Link ty} -> f ty
    | TVar {contents = Generic _} -> assert false
    | TVar ({contents = Unbound(other_id, other_level)} as other_tvar) ->
        if other_id = tvar_id then
          error "recursive types"
        else
          if other_level > tvar_level then
            other_tvar := Unbound(other_id, tvar_level)
        else
          ()
    | TApp(ty, ty_arg_list) ->
        f ty ;
        List.iter f ty_arg_list
    | TArrow(param_ty_list, return_ty) ->
        List.iter f param_ty_list ;
        f return_ty
    | TNum | TString | TUnit | TConst _ -> ()
  in
  f ty


(**
 * Unifies two types
 *
 * @return unit? or raises exception
 *)
let rec unify ty1 ty2 =
  if ty1 == ty2 then () else
    match (ty1, ty2) with
    | TConst name1, TConst name2 when name1 = name2 -> ()
    | TApp(ty1, ty_arg_list1), TApp(ty2, ty_arg_list2) ->
        unify ty1 ty2 ;
        List.iter2 unify ty_arg_list1 ty_arg_list2
    | TArrow(param_ty_list1, return_ty1), TArrow(param_ty_list2, return_ty2) ->
        List.iter2 unify param_ty_list1 param_ty_list2 ;
        unify return_ty1 return_ty2
    | TVar {contents = Link ty1}, ty2 | ty1, TVar {contents = Link ty2} -> unify ty1 ty2
    | TVar {contents = Unbound(id1, _)}, TVar {contents = Unbound(id2, _)} when id1 = id2 ->
        assert false (* There is only a single instance of a particular type variable. *)
    | TVar ({contents = Unbound(id, level)} as tvar), ty
    | ty, TVar ({contents = Unbound(id, level)} as tvar) ->
        occurs_check_adjust_levels id level ty ;
        tvar := Link ty
    | _, _ -> error ("cannot unify types " ^ show_ty ty1 ^ " and " ^ show_ty ty2)



let rec generalize level = function
  | TVar {contents = Unbound(id, other_level)} when other_level > level ->
      TVar (ref (Generic id))
  | TApp(ty, ty_arg_list) ->
      TApp(generalize level ty, List.map (generalize level) ty_arg_list)
  | TArrow(param_ty_list, return_ty) ->
      TArrow(List.map (generalize level) param_ty_list, generalize level return_ty)
  | TVar {contents = Link ty} -> generalize level ty
  | TVar {contents = Generic _} | TVar {contents = Unbound _} | TString _ | TNum _ | TConst _ as ty -> ty
  | _ -> failwith "generalize error"

let instantiate level ty =
  let id_var_map = Hashtbl.create 10 in
  let rec f ty = match ty with
    | TNum | TString | TUnit | TConst _ -> ty
    | TVar {contents = Link ty} -> f ty
    | TVar {contents = Generic id} ->
        begin
          try
            Hashtbl.find id_var_map id
          with Not_found ->
          let var = new_var level in
          Hashtbl.add id_var_map id var ;
              var
        end
    | TVar {contents = Unbound _} -> ty
    | TApp(ty, ty_arg_list) ->
        TApp(f ty, List.map f ty_arg_list)
    | TArrow(param_ty_list, return_ty) ->
        TArrow(List.map f param_ty_list, f return_ty)
  in
  f ty


let rec match_fun_ty num_params = function
  | TArrow(param_ty_list, return_ty) ->
      if List.length param_ty_list <> num_params then
        error "unexpected number of arguments"
      else
        param_ty_list, return_ty
  | TVar {contents = Link ty} -> match_fun_ty num_params ty
  | TVar ({contents = Unbound(id, level)} as tvar) ->
      let param_ty_list =
        let rec f = function
          | 0 -> []
          | n -> new_var level :: f (n - 1)
        in
        f num_params
      in
      let return_ty = new_var level in
      tvar := Link (TArrow(param_ty_list, return_ty)) ;
      param_ty_list, return_ty
  | _ -> error "expected a function"


(**
 * Infer types
 *
 * @param env
 * @param level int
 * @param exprs expr list
 * @return ty
 *)
let rec infer env level (exprs : expr_ list) =
  Env.dump env;
  match exprs with
  | [] ->
      TUnit
  | String (_, _) :: [] ->
      TString
  | Int (_, _) :: [] | Float (_, _) :: [] ->
      TNum
  (*
  | Var name ->
      begin
        try
          instantiate level (Env.lookup env name)
        with Not_found -> error ("variable " ^ name ^ " not found")
      end
  | Fun(param_list, body_expr) ->
      let param_ty_list = List.map (fun _ -> new_var level) param_list in
      let fn_env = List.fold_left2
        (fun env param_name param_ty -> Env.extend env param_name param_ty)
        env param_list param_ty_list
      in
      let return_ty = infer fn_env level body_expr in
      TArrow(param_ty_list, return_ty)
  *)
  (*| Let(var_name, value_expr, body_expr) ->*)
  (*| Lvar(var_name, value_expr) :: tail ->*)
  | Binop (Eq None, (_, Lvar (_, var_name)), (_, (value_expr))) :: tail ->
      let var_ty = infer env (level + 1) [value_expr] in
      let generalized_ty = generalize level var_ty in
      let already_exists = try Env.lookup env var_name; true with
        | Not_found -> false
      in
      if already_exists then unify (Env.lookup env var_name) generalized_ty;
      infer (Env.extend env var_name generalized_ty) level tail
  (*
  | Call(fn_expr, arg_list) ->
      let param_ty_list, return_ty =
        match_fun_ty (List.length arg_list) (infer env level fn_expr)
      in
      List.iter2
        (fun param_ty arg_expr -> unify param_ty (infer env level arg_expr))
        param_ty_list arg_list
      ;
      return_ty
  *)
  | _ -> failwith "Not implemented: infer"

let _ =
  let ast1 = [Let("a", Num 10); Let("a", String "asd")] in
  let ast2 = [
    Ast.Binop ((Ast.Eq None), (Pos.none, (Ast.Lvar (Pos.none, "$a"))),
      (Pos.none, (Ast.String (Pos.none, "asd"))));
    Ast.Binop ((Ast.Eq None), (Pos.none, (Ast.Lvar (Pos.none, "$a"))),
      (Pos.none, (Ast.Int (Pos.none, "123"))))]

  in
  let result = infer Env.empty 0 ast2 in
  print_endline (show_ty result)

  (*
      let result =
        try
          Infer.reset_id () ;
                                let ty = Infer.infer Core.core 0 (Parser.expr_eof Lexer.token (Lexing.from_string code)) in
                                let generalized_ty = Infer.generalize (-1) ty in
                                OK (string_of_ty generalized_ty)
                                                      with Infer.Error msg ->
                                                        Fail (Some msg)
                                in
                                *)
(*
 (Ast.Stmt
    (Ast.Expr
       (<opaque>,
        Ast.Binop ((Ast.Eq None), (<opaque>, (Ast.Lvar (<opaque>, "$a"))),
          (<opaque>, (Ast.String (<opaque>, "asd")))))))]
*)
