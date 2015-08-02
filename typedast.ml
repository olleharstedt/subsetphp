(**
 * AST built up by type-inference, with types.
 * Used to generate LLVM IR.
 *
 * @author Olle Harstedt
 * @since 2015-07-29
 *)

type ty =
  | TNumber
  | TString
  | TBoolean
  | TUnit
  | TArrow of ty list * ty            (* function type: e.g. `(int, int) -> int` *)

and id = Pos.t * string
and pstring = Pos.t * string

and program = def list

and def =
  | Stmt of stmt
  | Fun of fun_

and stmt =
  | Expr of ty * expr
  | Block of block
  | If of expr * block * block

and block = stmt list

and expr = Pos.t * expr_
and expr_ =
  | Id of id * ty  (* Added type here *)
  | Lvar of id * ty
  | Number of float

and fun_param = {
  param_id : id;
  param_type : ty;
}

and fun_ = {
  f_name : id;
  f_params : fun_param list;
  f_ret : ty;
}
[@@deriving show]

(**
 * Create a typed fun from an untyped one
 *
 * @param env
 * @param Ast.f_param list
 * @return f_param list
 *)
let create_typed_params env (f_params : Ast.fun_param list) =
  let rec aux params typed_params = match params with
  | [] ->
      typed_params
  | param :: params ->
      aux params typed_params
  in
  aux f_params []

