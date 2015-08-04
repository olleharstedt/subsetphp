(**
 * Code taken from: https://github.com/tomprimozic/type-systems
 *)

open Ast
open Printf

type name = string
[@@deriving show]

(*
type expr =
  | Var of name                           (* variable *)
  | Call of expr * expr list              (* application *)
  | Fun of name * expr list              (* abstraction, name of function and args *)
  (*| Let of name * expr * expr           (* let *)*)
  | Let of name * expr                    (* PHP variables don't have scope *)
  | Num of int                            (* PHP can't infer int or float, use num *)
  | String of string
*)

(** PHP programs are lists of expressions/statements *)
type progrem = expr list

type id = int
[@@deriving show]
type level = int
[@@deriving show]

type ty =
  | TConst of name                    (* type constant: `int` or `bool`. TODO: Used? *)
  | TApp of ty * ty list              (* type application: `list[int]` *)
  | TArrow of ty list * ty            (* function type: e.g. `(int, int) -> int` *)
  | TVar of tvar ref                  (* type variable *)
  | TNum
  | TString
  | TBoolean
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
exception Not_implemented of string

let error msg = raise (Error msg)


module Env = struct
  module StringMap = Map.Make (String)
  type env = {
    map : ty StringMap.t;
    return_ty : ty option;
  }

  (* ty StringMap.t * ty option  (* normal mappings * return type *)*)

  let empty : env = {map = StringMap.empty; return_ty = None}
  let extend env name ty =
    let new_mapping = StringMap.add name ty (env.map) in
    {env with map = new_mapping}
  let lookup env name = StringMap.find name env.map
  let new_return_type env ty : env =
    {env with return_ty = ty}

  (**
   * Print environment
   *
   * @return unit
   *)
  let dump (env : env) =
    print_endline "[ env = ";
    StringMap.iter (fun key value ->
      print_endline (sprintf "    %s : %s" key (show_ty value))
    ) env.map;
    print_endline (sprintf "    return type = %s" (match env.return_ty with Some ty -> show_ty ty | None -> "None"));
    print_endline "]"
end

(**
 * Map from ty to Typedast.ty
 *
 * @param ty
 * @return Typedast.ty
 *)
let rec ty_of_ty typ =
  match typ with
  | TConst _ | TApp _ | TVar _ -> failwith "ty_of_ty: Has no correspondance in Typedast"
  | TNum -> Typedast.TNumber
  | TString -> Typedast.TString
  | TBoolean -> Typedast.TBoolean
  | TUnit -> Typedast.TUnit
  | TArrow (args, ret) ->
      Typedast.TArrow ([], ty_of_ty ret)

(**
 * Binary operation to Typedast op
 *
 * @param bop
 * @return Typedast.bop
 *)
let bop_to_typed bop =
  match bop with
  | Plus -> Typedast.Plus
  | Minus -> Typedast.Minus
  | Star -> Typedast.Star
  | Slash -> Typedast.Slash
  | Starstar -> Typedast.Starstar
  | Percent -> Typedast.Percent
  | _ -> raise (Not_implemented "bop_to_typed")

(**
 * Turn type expr into expr_
 * Strip Pos, that is
 *)
let expr_of_expr_ expr =
  match expr with
  | (pos, expr_) -> expr_

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
    | TNum | TString | TBoolean | TUnit | TConst _ -> ()
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
    | _, _ ->
        let bt = Printexc.get_backtrace () in
        print_endline bt;
        error ("cannot unify types " ^ show_ty ty1 ^ " and " ^ show_ty ty2)



let rec generalize level = function
  | TVar {contents = Unbound(id, other_level)} when other_level > level ->
      TVar (ref (Generic id))
  | TApp(ty, ty_arg_list) ->
      TApp(generalize level ty, List.map (generalize level) ty_arg_list)
  | TArrow(param_ty_list, return_ty) ->
      TArrow(List.map (generalize level) param_ty_list, generalize level return_ty)
  | TVar {contents = Link ty} -> generalize level ty
  | TVar {contents = Generic _} | TVar {contents = Unbound _} | TString | TNum | TBoolean | TUnit | TConst _ as ty -> ty

  (*| ty -> failwith (sprintf "generalize error: %s" (show_ty ty))*)


let instantiate level ty =
  let id_var_map = Hashtbl.create 10 in
  let rec f ty = match ty with
    | TNum | TString | TBoolean | TUnit | TConst _ -> ty
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

(**
 * Gives type of function?
 *
 * @param num_params      Number of parameters to function
 * @param ty ?
 * @return ty list * ty   List of param types and return type
 *)
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
 * Infer type of program
 * A program is a list of definitions
 *
 * @param level ?
 * @param defs program
 * @return Typedast.program
 *)
let rec infer_program level (defs : def list) : Typedast.program =
  let env = Env.empty in
  (**
   * @param typed_program Collect typed subexpressions and return
   *)
  let rec aux env level defs (typed_program : Typedast.program) =
    Env.dump env;
    match defs with
    | [] ->
        typed_program
    | Stmt stmt :: tail ->
        let (typed_stmt, env) = infer_stmt env level stmt in
        aux env level tail (typed_program @ [Typedast.Stmt typed_stmt])
    | Fun fun_ :: tail ->
        let open Namespace_env in
        let (_, fn_name) = fun_.f_name in
        let namespace_name = fun_.f_namespace.ns_name in
        let fn_name = String.sub fn_name 1 (String.length fn_name - 1) in  (* Strip leading \ (namespace thing) *)

        begin match namespace_name with
          | Some _ -> failwith "namespaces not implemented"
          | None -> ()
        end;

        let (typed_fn, env, fn_ty) = infer_fun env level fun_ in
        let env = Env.extend env fn_name fn_ty in
        (*infer_exprs (Env.extend env var_name generalized_ty) level tail;*)
        aux env level tail (typed_program @ [typed_fn])
    | _ -> failwith "Not implemented: infer_program"
  in
  aux env level defs []

(**
 * Small helper function to infer type of stmt list, which
 * is of course unit, and update the env.
 *
 * @return Typedast.stmt list * env
 *)
and infer_block env level (stmts : stmt list) : Typedast.block * Env.env =
  let _env = ref env in
  let typed_stmts = ref [] in
  for i = 0 to List.length stmts - 1 do
    let stmt = List.nth stmts i in
    let (typed_stmt, env) = infer_stmt env level stmt in
    _env := env;
    typed_stmts := !typed_stmts @ [typed_stmt];
  done;
  !typed_stmts, !_env

(**
 * Infer statement
 * All statements return type TUnit
 *
 * @param env
 * @param level int
 * @param stmt stmt
 * @return Typedast.program * env
 *)
and infer_stmt (env : Env.env) level (stmt : stmt) : (Typedast.stmt * Env.env) =
  Env.dump env;
  match stmt with
  | Expr expr ->

      let (typed_expr, env, ty) = infer_expr env 0 expr in

      (* All expressions in statements must return unit *)
      unify TUnit ty;

      Typedast.Expr (Typedast.TUnit, typed_expr), env

      (*infer_stmts env level tail typed_stmts*)
      (*
  | Noop ->
      (typed_stmts, env)
*)
  | Block stmts ->
      let (block, env) = infer_block env level stmts in
      Typedast.Block block, env

  | If (e, block1, block2) ->

      let (typed_stmts_block1, env) = infer_block env level block1 in
      let (typed_stmts_block2, env) = infer_block env level block2 in
      let (typed_expr, env, e_ty) = infer_expr env level e in

      (* If-clause most be bool *)
      unify TBoolean e_ty;

      Typedast.If (typed_expr, typed_stmts_block1, typed_stmts_block2), env

  | Return (pos, expr_opt) ->
      let open Env in
      (match expr_opt with
      | None ->
          (match env.return_ty with
          | Some ty ->
              unify ty TUnit
          | None ->
              ()
          );
          let env = {env with return_ty = Some TUnit} in
          Typedast.Return (Typedast.TUnit, pos, None), env
      | Some expr ->
          let (typed_expr, env, return_ty) = infer_expr env level expr in
          (match env.return_ty with
          | Some ty ->
              unify return_ty ty
          | None ->
              ()
          );
          let env = Env.new_return_type env (Some return_ty) in
          Typedast.Return (ty_of_ty return_ty, pos, Some typed_expr), env
      )
  | stmt -> failwith (sprintf "Not implemented: infer_stmt: %s" (show_stmt stmt))

(**
 * @return Typedast.def * env * ty
 *)
and infer_fun (env : Env.env) level fun_ : Typedast.def * Env.env * ty =
  let open Env in
  (*let param_ty_list = List.map (fun _ -> new_var level) param_list in*)
  let param_list = List.map (fun param -> match param.param_id with
    | _, name -> name
  ) fun_.f_params in
  let param_ty_list = List.map (fun _ -> new_var level) param_list in

  (* New scope for function *)
  (* TODO: Global variables? *)
  let empty_env = Env.empty in

  let fn_env = List.fold_left2
    (fun env param_name param_ty -> Env.extend env param_name param_ty)
    empty_env param_list param_ty_list
  in
  let body_expr = fun_.f_body in

  (* Get the return type from the body of the function *)
  let (_, fn_env) = infer_block fn_env level body_expr in
  let return_type = match fn_env.return_ty with
  | Some ty -> ty
  | None -> TUnit
  in

  print_endline (sprintf "return type = %s" (show_ty return_type));

  let typed_fn = Typedast.(Fun {
    f_name = fun_.f_name;  (* TODO: Fix pos *)
    f_params = Typedast.create_typed_params env fun_.f_params;
    f_ret = ty_of_ty return_type;
  }) in

  (typed_fn, env, TArrow(param_ty_list, return_type))

(**
 * Infer types for an expression and return typed AST, new env and expr type
 *
 * @param env
 * @param level int
 * @param exprs expr list
 * @return Typedast.expr * env * ty
 *)
and infer_expr (env : Env.env) level expr : Typedast.expr * Env.env * ty =
  Env.dump env;
  match expr with
  | p, String (pos, str) ->
      (p, Typedast.String (pos, str)), env, TString
  | p, Int (pos, pstring) ->
      (p, Typedast.Int (pos, pstring)), env, TNum
  | p, Float (pos, pstring) ->
      (p, Typedast.Float (pos, pstring)), env, TNum
  | p, Binop (Eq None, (pos_lvar, Lvar (pos_var_name, var_name)), value_expr) ->
      let (typed_value_expr, env, value_ty) = infer_expr env (level + 1) value_expr in

      (* Abort if right-hand is unit *)
      if value_ty = TUnit then failwith "Right-hand can't evaluate to void";

      let generalized_ty = generalize level value_ty in
      let already_exists = try ignore (Env.lookup env var_name); true with
        | Not_found -> false
      in

      let env = if already_exists then begin
        unify (Env.lookup env var_name) generalized_ty ;
        env
      end else
        Env.extend env var_name value_ty
      in

      let typed_lvar = Typedast.Lvar ((pos_var_name, var_name), ty_of_ty value_ty) in
      (p, Typedast.Binop (Typedast.Eq None, (pos_lvar, typed_lvar), typed_value_expr, Typedast.TUnit)), env, TUnit

  (* Numerical op *)
  | p, Binop (bop, expr1, expr2) when is_numerical_op bop ->
      let (typed_bop, typed_expr1, typed_expr2, env) = infer_numberical_op env level bop expr1 expr2 in
      (p, Typedast.Binop (typed_bop, typed_expr1, typed_expr2, Typedast.TNum)), env, TNum

  | p, Lvar (pos, var_name) ->
      let var_type = try Some (Env.lookup env var_name) with | Not_found -> None in
      let var_type = (match var_type with
      | None -> failwith (sprintf "Can't use variable before it's defined: %s" var_name)
      | Some var_type -> var_type)
      in
      (p, Typedast.Lvar ((pos, var_name), ty_of_ty var_type)), env, var_type

  | p, Call ((pos1, Id (pos_fn, fn_name)), arg_list, dontknow) ->
      print_endline fn_name;
      let fn_ty = try Some (Env.lookup env fn_name) with | Not_found -> None in
      let return_ty = (match fn_ty with
      | Some ty ->
          (match ty with
          | TArrow (args, return_ty) ->
              List.iter2
                (fun param_ty arg_expr ->
                  let (typed_arg_expr, env, ty) = infer_expr env level arg_expr in
                  unify param_ty ty
                )
                args arg_list
              ;
              return_ty
          | _ ->
              failwith "Not a function?"
          )
      | None ->
          (* Infer function type here
           * How much can we infer from a function usage in PHP? Lack of syntax for optional argument
           * screw things up
           *)
          failwith "not implemented: infer function type before definition"
      ) in
      let fn = (fun expr ->
          let (typed_expr, env, ty) = infer_expr env level expr in
          typed_expr
      ) in
      let typed_arg_list = List.map fn arg_list in
      let typed_dontknow = List.map fn dontknow in
      let typed_call = Typedast.Call ((pos1, Typedast.Id ((pos_fn, fn_name), ty_of_ty return_ty)), typed_arg_list, typed_dontknow) in
      (p, typed_call), env, return_ty
  | expr -> failwith (sprintf "Not implemented: infer_exprs: %s" (show_expr expr))

(**
 * Infer numerical binary operations, like +, -
 *
 * @return Typedast.bop * env
 *)
and infer_numberical_op env level bop expr1 expr2 =
  match bop with
  | Plus | Minus | Star | Slash | Starstar | Percent as bop ->
      let (typed_expr1, env, expr1_ty) = infer_expr env level expr1 in
      let (typed_expr2, env, expr2_ty) = infer_expr env level expr2 in
      unify expr1_ty TNum;
      unify expr2_ty TNum;
      let typed_bop = bop_to_typed bop in
      typed_bop, typed_expr1, typed_expr2, env
  | _ ->
      failwith "Not implemented: infer_bop"

and is_numerical_op = function
  | Plus | Minus | Star | Slash | Starstar | Percent ->
      true
  | _ ->
      false

  (*print_endline (show_ty result_ty)*)

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

 $a = 'asd';

 (Ast.Stmt
    (Ast.Expr
       (<opaque>,
        Ast.Binop ((Ast.Eq None), (<opaque>, (Ast.Lvar (<opaque>, "$a"))),
          (<opaque>, (Ast.String (<opaque>, "asd")))))))]
*)

(*

 function foo() { $a = 10; }

 (Ast.Fun
    { Ast.f_mode = FileInfo.Mpartial; f_tparams = []; f_ret = None;
      f_ret_by_ref = false; f_name = (<opaque>, "\\foo"); f_params = [
      ];
      f_body = [(Ast.Expr
                   (<opaque>,
                    Ast.Binop ((Ast.Eq None),
                      (<opaque>, (Ast.Lvar (<opaque>, "$a"))),
                      (<opaque>, (Ast.Int (<opaque>, "10"))))))];
      f_user_attributes = []; f_mtime = 0.; f_fun_kind = Ast.FSync;
      f_namespace = { Namespace_env.ns_uses = <opaque>; ns_name = None } })]
*)

(*

$a = 10;

[(Typedast.Stmt
    Typedast.Expr (Typedast.TUnit,
      (<opaque>,
       Typedast.Binop ((Typedast.Eq None),
         (<opaque>, Typedast.Lvar ((<opaque>, "$a"), Typedast.TNumber)),
         (<opaque>, (Typedast.Int (<opaque>, "10"))), Typedast.TUnit))))]
*)
