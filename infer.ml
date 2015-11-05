(**
 * Code taken from: https://github.com/tomprimozic/type-systems
 *)

open Ast
open Printf

exception Not_implemented of string
exception MapMergeException of string

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
  | TNumber
  | TString
  | TStruct
  | TBoolean
  | TUnit
  | TUnresolved  (* Not yet resolved, as in struct which need to be used to know the type *)
[@@deriving show]

(** TODO: Explain? *)
(** Type variable *)
and tvar =
  | Unbound of id * level
  | Link of ty  (* alias? *)
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
exception Unify_error of string * ty * ty
exception Not_implemented of string

let error msg = raise (Error msg)

(**
 * Return file and line information used in error messages
 *
 * @param pos Pos.pos
 * @return string
 *)
let get_pos_msg pos =
  let line, start, end_ = Pos.info_pos pos in
  sprintf "File %S, line %d, characters %d-%d"
    (snd Pos.(pos.pos_file)) line start end_

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
   * Return all functions from an environment 
   * Usefule when injecting namespaces into a function env
   *
   * @param env
   * @return env
   *)
  let get_functions env : env =
    let new_map = StringMap.filter (fun key value ->
      match value with
      | TArrow _ -> true
      | _ -> false
    ) env.map
    in
    {env with map = new_map}

  (**
   * Add env src to env dest
   *
   * @param src env
   * @param dest env
   * @return src + dest env
   *)
  let merge src dest : env =
    let new_map = StringMap.merge (fun key src_val dest_val ->
      match src_val, dest_val with
      | None, None -> None
      | Some a, None -> Some a
      | None, Some a -> Some a
      | Some a, Some b ->
          raise (MapMergeException "Can't merge environments - one key exists in both environments")
    ) src.map dest.map
    in 
    {dest with map = new_map}

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
  | TVar tvar_ref ->
      let tvar = !tvar_ref in
      begin match tvar with 
        | Link ty ->
            ty_of_ty ty
        | _ -> Typedast.TUnknown
      end
  | TConst _ | TApp _ -> failwith "ty_of_ty: Has no correspondance in Typedast"
  | TNumber -> Typedast.TNumber
  | TString -> Typedast.TString
  | TBoolean -> Typedast.TBoolean
  | TUnit -> Typedast.TUnit
  | TStruct -> Typedast.TStruct
  | TUnresolved -> failwith "type is unresolved"
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

(**
 * ?
 *)
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
    | TNumber | TString | TStruct | TBoolean | TUnit | TConst _ | TUnresolved-> ()
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
        raise (Unify_error (("Cannot unify types " ^ show_ty ty1 ^ " and " ^ show_ty ty2), ty1, ty2))

(** TODO: What does this do? What is level? *)
let rec generalize level = function
  | TVar {contents = Unbound(id, other_level)} when other_level > level ->
      TVar (ref (Generic id))
  | TApp(ty, ty_arg_list) ->
      TApp(generalize level ty, List.map (generalize level) ty_arg_list)
  | TArrow(param_ty_list, return_ty) ->
      TArrow(List.map (generalize level) param_ty_list, generalize level return_ty)
  | TVar {contents = Link ty} -> generalize level ty
  | TVar {contents = Generic _} | TVar {contents = Unbound _} | TString | TStruct | TNumber | TBoolean | TUnit | TUnresolved | TConst _ as ty -> ty

  (*| ty -> failwith (sprintf "generalize error: %s" (show_ty ty))*)


let instantiate level ty =
  let id_var_map = Hashtbl.create 10 in
  let rec f ty = match ty with
    | TNumber | TString | TStruct | TBoolean | TUnit | TConst _ | TUnresolved -> ty
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
and infer_program level (defs : def list) : Typedast.program =
  let env = Env.empty in

  (* Add core functions *)
  let printd_ty = TArrow ([TNumber], TUnit) in
  let env = Env.extend env "printd" printd_ty in
  let prints_ty = TArrow ([TString], TUnit) in
  let env = Env.extend env "prints" prints_ty in

  (**
   * Helper function to infer types of defs
   * Stmts, funs, classes...
   *
   * @param typed_program Collect typed subexpressions and return
   * @return typed_program
   *)
  let rec aux env level defs (typed_program : Typedast.program) =
    (*Env.dump env;*)
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

        let env_with_fn = Env.get_functions env in
        let (typed_fn, env, fn_ty) = infer_fun env_with_fn level fun_ in
        let env = Env.extend env fn_name fn_ty in
        (*infer_exprs (Env.extend env var_name generalized_ty) level tail;*)
        aux env level tail (typed_program @ [typed_fn])
    | Class class_ :: tail ->
        let (typed_class, env, class_ty) = infer_class env level class_ in
        let (_, class_name) = class_.c_name in
        let class_name = String.sub class_name 1 (String.length class_name - 1) in  (* Strip leading \ (namespace thing) *)
        let env = Env.extend env class_name class_ty in
        (*Env.dump env;*)
        aux env level tail (typed_program @ [typed_class])
    | _ -> raise (Not_implemented "infer_program")
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
    let (typed_stmt, env) = infer_stmt !(_env) level stmt in
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
  (*Env.dump env;*)
  match stmt with
  | Expr expr ->

      let (typed_expr, env, ty) = infer_expr env 0 expr in

      (* All expressions in statements must return unit *)
      unify TUnit ty;

      Typedast.Expr (Typedast.TUnit, typed_expr), env

      (*infer_stmts env level tail typed_stmts*)
  | Noop ->
      (Typedast.Noop, env)
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

  | For ((p, Expr_list [start]), (p2, Expr_list [end_]), (p3, Expr_list [step]), body) ->  (* TODO: How to infer step? *)

      (* Check so start contains an assignement *)
      begin match start with
        | p, Binop (Eq None, (p4, Lvar (p5, var_name)), _) ->
            let already_exists = try ignore (Env.lookup env var_name); true with
              | Not_found -> false
            in
            if already_exists then failwith "For-loop variable must not already exist in environment";
        | _ ->
            failwith "For start must have assignment, like $i = 0"
      end;

      let (typed_start, env, start_ty) = infer_expr env level start in
      let (typed_end_, env, end_ty) = infer_expr env level end_ in

      if end_ty != TBoolean then failwith "Condition in for loop must evaluate to true or false";

      let (typed_step, env, step_ty) = infer_expr env level step in
      let (typed_body, env) = infer_block env level body in

      Typedast.For (typed_start, typed_end_, typed_step, typed_body), env

  | For _ ->
      failwith "Illegal form of for loop - only one expression in each block is allowed"

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
          Typedast.Return (pos, None, Typedast.TUnit), env
      | Some expr ->
          let (typed_expr, env, return_ty) = infer_expr env level expr in
          (match env.return_ty with
          | Some ty ->
              unify return_ty ty
          | None ->
              ()
          );
          let env = Env.new_return_type env (Some return_ty) in
          Typedast.Return (pos, Some typed_expr, ty_of_ty return_ty), env
      )
  | stmt -> raise (Not_implemented (sprintf "infer_stmt: %s" (show_stmt stmt)))

(**
 * Create a typed fun from an untyped one
 *
 * @param env
 * @param Ast.f_param list
 * @return f_param list
 *)
and create_typed_params env (f_params : Ast.fun_param list) =
  printf "create_typed_params";
  (*Env.dump env;*)
  let open Typedast in
  let rec aux (params : Ast.fun_param list) typed_params =
    match params with
      | [] ->
          typed_params
      | {Ast.param_id = (pos, name)} :: params ->
          printf "name = %s" name;
          let already_exists = try ignore (Env.lookup env name); true with
            | Not_found -> false
          in
          let typed_param  = if already_exists then begin
            let ty = Env.lookup env name in
            {
              param_id = (pos, name);
              param_type = ty_of_ty ty;
            }
            end else begin
            {
              param_id = (pos, name);
              param_type = TUnknown;
            }
          end in
          aux params (typed_params @ [typed_param])
  in
  aux f_params []

(**
 * Infer type of function
 *
 * @param env Env with functions in same scope as this function
 * @param level ?
 * @param fun_
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
  (* TODO: Global variables? Functions? *)
  let empty_env = Env.empty in
  let new_env = Env.merge env empty_env in

  let fn_env = List.fold_left2
    (fun env param_name param_ty -> Env.extend env param_name param_ty)
    new_env param_list param_ty_list
  in
  let body_expr = fun_.f_body in

  (* Get the return type from the body of the function *)
  let (typed_body_expr, fn_env) = infer_block fn_env level body_expr in
  let return_type = match fn_env.return_ty with
    | Some ty -> ty
    | None -> TUnit
  in

  print_endline (sprintf "return type = %s" (show_ty return_type));

  let typed_fn = Typedast.(Fun {
    f_name = fun_.f_name;  (* TODO: Fix pos *)
    f_params = create_typed_params fn_env fun_.f_params;
    f_ret = ty_of_ty return_type;
    f_body = typed_body_expr;
  }) in

  (typed_fn, env, TArrow(param_ty_list, return_type))

(**
 * Infer class
 *
 * Class can be struct
 *
 * @param env
 * @param level ?
 * @param class_
 * @return Typedast.class * env * ty
 *)
and infer_class (env : Env.env) level class_ : Typedast.def * Env.env * ty =
  match class_ with
  (*
[(Ast.Class
    { Ast.c_mode = FileInfo.Mpartial; c_user_attributes = []; c_final = true;
      c_kind = Ast.Cnormal; c_is_xhp = false; c_name = (<opaque>, "\\Point");
      c_tparams = []; c_extends = []; c_implements = [];
      c_body = [Ast.ClassVars ([Ast.Public], None, [((<opaque>, "x"), None)]);
                Ast.ClassVars ([Ast.Public], None, [((<opaque>, "y"), None)])];
      c_namespace = { Namespace_env.ns_uses = <opaque>; ns_name = None };
      c_enum = None })]
*)
  | {c_final = true; c_body} when c_body_is_only_public c_body -> 
      let typed_struct = c_body_to_struct c_body in
      Typedast.Struct {Typedast.fields = typed_struct}, env, TStruct
  | _ ->
      raise (Not_implemented ("This class type is not implemented: " ^ show_def (Class class_)))

(**
 * Return true if class body contains only
 * public fields
 *
 * @param c_body
 * @return bool
 *)
and c_body_is_only_public c_body =
  List.for_all (fun class_elt ->
    match class_elt with
    | ClassVars ([Public], None, [(_, None)]) ->
        true
    | _ ->
        false
  ) c_body

(**
 * For a struct, take all fields
 * and make them typed struct fields
 *
 * @param c_body
 * @return string, ty list - string = name of field
 *)
and c_body_to_struct (c_body : class_elt list) =
  List.map (fun class_elt ->
    match class_elt with
    | ClassVars ([Public], None, [((pos, name), None)]) ->
      name, Typedast.TUnknown  (* Not known before-hand *)
    | _ ->
        failwith "Internal error: illegal class element"
  ) c_body

(**
 * Infer types for an expression and return typed AST, new env and expr type
 *
 * @param env
 * @param level int
 * @param exprs expr list
 * @return Typedast.expr * env * ty
 *)
and infer_expr (env : Env.env) level expr : Typedast.expr * Env.env * ty =
  (*Env.dump env;*)
  match expr with
  | p, True ->
      (p, Typedast.True), env, TBoolean
  | p, False ->
      (p, Typedast.False), env, TBoolean
  | p, String (pos, str) ->
      (p, Typedast.String (pos, str)), env, TString
  | p, String2 (pos, str) ->
      failwith "Only strings with '' are supported"
  | p, Int (pos, pstring) ->
      (p, Typedast.Int (pos, pstring)), env, TNumber
  | p, Float (pos, pstring) ->
      (p, Typedast.Float (pos, pstring)), env, TNumber

  (* > *)
  | p, Binop (Gt, lexpr, rexpr) ->
      let (typed_lexpr, _, lexpr_ty) = infer_expr env (level + 1) lexpr in
      let (typed_rexpr, _, rexpr_ty) = infer_expr env (level + 1) rexpr in

      (* Check so that left hand and right hand are numeric *)
      unify lexpr_ty TNumber;
      unify rexpr_ty TNumber;

      (p, Typedast.Binop (Typedast.Gt, typed_lexpr, typed_rexpr, Typedast.TBoolean)), env, TBoolean

  (* < *)
  | p, Binop (Lt, lexpr, rexpr) ->
      let (typed_lexpr, _, lexpr_ty) = infer_expr env (level + 1) lexpr in
      let (typed_rexpr, _, rexpr_ty) = infer_expr env (level + 1) rexpr in

      (* Check so that left hand and right hand are numeric *)
      unify lexpr_ty TNumber;
      unify rexpr_ty TNumber;

      (p, Typedast.Binop (Typedast.Lt, typed_lexpr, typed_rexpr, Typedast.TBoolean)), env, TBoolean

  (* === *)
  | p, Binop (EQeqeq, (lpos, lexpr), (rpos, rexpr)) ->
      let (typed_lexpr, _, lexpr_ty) = infer_expr env (level + 1) (lpos, lexpr) in
      let (typed_rexpr, _, rexpr_ty) = infer_expr env (level + 1) (rpos, rexpr) in

      (* Check so that left hand and right hand are the same type *)
      unify lexpr_ty rexpr_ty;

      (p, Typedast.Binop (Typedast.EQeqeq (ty_of_ty lexpr_ty), typed_lexpr, typed_rexpr, Typedast.TBoolean)), env, TBoolean

  (* += and -= *)
  | p, Binop (Eq (Some Plus as op), (pos_lvar, Lvar (pos_var_name, var_name)), value_expr)
  | p, Binop (Eq (Some Minus as op), (pos_lvar, Lvar (pos_var_name, var_name)), value_expr) ->

      let already_exists = try ignore (Env.lookup env var_name); true with
        | Not_found -> false
      in

      (* Left hand-side must already be defined for this operator to work *)
      if already_exists then begin

        (* Check so left hand-side is a number *)
        let lvar_ty = Env.lookup env var_name in
        unify (Env.lookup env var_name) TNumber;

        let (typed_value_expr, env, value_ty) = infer_expr env (level + 1) value_expr in

        (* Right hand-side should always unify to number *)
        let generalized_ty = generalize level value_ty in
        unify TNumber generalized_ty;

        let typed_lvar = (pos_lvar, Typedast.Lvar ((pos_var_name, var_name), ty_of_ty lvar_ty)) in

        let typed_op = match op with
          | Some Plus -> Typedast.Plus
          | Some Minus -> Typedast.Minus
          | _ -> failwith "internal error 1508221315"
        in

        (p, Typedast.Binop (Typedast.Eq (Some typed_op), typed_lvar, typed_value_expr, Typedast.TNumber)), env, TUnit

      end else
        failwith "Left hand-side is not defined"

  (* =, Assignment *)
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
      (p, Typedast.Binop (typed_bop, typed_expr1, typed_expr2, Typedast.TNumber)), env, TNumber

  (* . concatenation *)
  (*| p, Binop (Dot, (p, (Lvar (p, "$a"))), (p, (String (p, "qwe")))) ->*)
  | p, Binop (Dot, expr1, expr2) ->
      let (typed_expr1, env, expr1_ty) = infer_expr env (level + 1) expr1 in
      let (typed_expr2, env, expr2_ty) = infer_expr env (level + 1) expr2 in

      begin try begin
        unify expr1_ty TString;
        unify expr2_ty TString;
      end with
        | Unify_error (msg, ty1, ty2) ->
            let pos_msg = get_pos_msg p in
            raise (Error (sprintf "%s: Can only concatenate strings" pos_msg))
      end;

      (* Binop of bop * expr * expr * ty *)
      (p, (Typedast.Binop (Typedast.Dot, typed_expr1, typed_expr2, Typedast.TString))), env, TString

  | p, Lvar (pos, var_name) ->
      let var_type = try Some (Env.lookup env var_name) with | Not_found -> None in
      let var_type = (match var_type with
        | None -> failwith (sprintf "Can't use variable before it's defined: %s" var_name)
        | Some var_type -> var_type)
      in
      (p, Typedast.Lvar ((pos, var_name), ty_of_ty var_type)), env, var_type

  (* Hard-code support for print *)
  (* Print for number *)
  | p, Call ((pos1, Id (pos_fn, "echo")), [(pos_arg, Int int_string)], dontknow) ->
    (* Function to get typed expression *)
    let get_typed_expr = (fun expr ->
        let (typed_expr, env, ty) = infer_expr env level expr in
        typed_expr
    ) in
    let typed_dontknow = List.map get_typed_expr dontknow in
    (p, Typedast.Call ((pos1, Typedast.Id ((pos_fn, "printd"), Typedast.TUnit)), [(pos_arg, Typedast.Int int_string)], typed_dontknow)), 
      env,
      TUnit

  (* Print for string *)
  | p, Call ((pos1, Id (pos_fn, "echo")), [(pos_arg, String str)], dontknow) ->
    (* Function to get typed expression *)
    let get_typed_expr = (fun expr ->
        let (typed_expr, env, ty) = infer_expr env level expr in
        typed_expr
    ) in
    let typed_dontknow = List.map get_typed_expr dontknow in
    (p, Typedast.Call ((pos1, Typedast.Id ((pos_fn, "prints"), Typedast.TUnit)), 
      [(pos_arg, Typedast.String str)], typed_dontknow)), 
      env, 
      TUnit

  (* Print for arbitrary expressions *)
  | p, Call ((pos1, Id (pos_fn, "echo")), [expr], dontknow) ->
    (* Function to get typed expression *)
    let get_typed_expr = (fun expr ->
        let (typed_expr, env, ty) = infer_expr env level expr in
        typed_expr
    ) in
    let typed_expr, env, ty = infer_expr env level expr in
    begin match ty with
      | TNumber ->
          let typed_dontknow = List.map get_typed_expr dontknow in
          (p, Typedast.Call ((pos1, Typedast.Id ((pos_fn, "printd"), Typedast.TUnit)), 
            [typed_expr], typed_dontknow)), 
            env, 
            TUnit
      | TString ->
          let typed_dontknow = List.map get_typed_expr dontknow in
          (p, Typedast.Call ((pos1, Typedast.Id ((pos_fn, "prints"), Typedast.TUnit)), 
            [typed_expr], typed_dontknow)), 
            env, 
            TUnit
      | ty ->
          raise (Not_implemented (sprintf "echo not supported for type %s" (show_ty ty)))
    end

  (* Function call *)
  | p, Call ((pos1, Id (pos_fn, fn_name)), arg_list, dontknow) ->
      let fn_ty = try Some (Env.lookup env fn_name) with | Not_found -> None in
      let return_ty = (match fn_ty with
        | Some ty ->
            printf "fn_ty = %s\n" (show_ty ty);
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
            failwith (sprintf "not implemented: infer function type before definition: %s" fn_name)
      ) in
      (* Function to get typed expression *)
      let get_typed_expr = (fun expr ->
          let (typed_expr, env, ty) = infer_expr env level expr in
          typed_expr
      ) in
      let typed_arg_list = List.map get_typed_expr arg_list in
      let typed_dontknow = List.map get_typed_expr dontknow in
      let typed_call = Typedast.Call ((pos1, Typedast.Id ((pos_fn, fn_name), ty_of_ty return_ty)), typed_arg_list, typed_dontknow) in
      (p, typed_call), env, return_ty
   (*Ast.New ((<opaque>, (Ast.Id (<opaque>, "Point"))), [], []))))));*)
  | p, New ((pos, (Ast.Id (pos2, object_name))), [], []) ->
      let object_ty = try Some (Env.lookup env object_name) with | Not_found -> None in
      let _ = begin match object_ty with
        | Some ty ->
            ()
        | None ->
            failwith (sprintf "not implemented: infer object type before definition: %s" object_name)
      end in
      (p, Typedast.New 
        (
          (pos, 
            (Typedast.Id 
              ((pos2, object_name),
              Typedast.TStruct
              )
            )
          ), 
          [], 
          [],
          Typedast.TUnknown
        )
      ), env, new_var 0
      (*(p, Typedast.New ((pos, (Typedast.Id (pos2, object_name))), [], [], object_ty)), env, TUnknown*)

(*
(<opaque>,
 Ast.Obj_get ((<opaque>, (Ast.Lvar (<opaque>, "$a"))),
   (<opaque>, (Ast.Id (<opaque>, "x"))), Ast.OG_nullthrows)),
*)

  | p, Obj_get ((pos2, (Lvar (pos3, var_name))), (pos4, (Id (pos5, obj_name))), og_null_flavor) ->
      (p, Typedast.True), env, TBoolean

  | expr -> raise (Not_implemented (sprintf "infer_exprs: %s" (show_expr expr)))

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
      unify expr1_ty TNumber;
      unify expr2_ty TNumber;
      let typed_bop = bop_to_typed bop in
      typed_bop, typed_expr1, typed_expr2, env
  | _ ->
      raise (Not_implemented "infer_bop")

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
