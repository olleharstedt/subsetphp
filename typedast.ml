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
  | Return of ty * Pos.t * expr option
  | Noop

and block = stmt list

and expr = Pos.t * expr_
and expr_ =
  | True
  | False
  | Id of id * ty
  | Lvar of id * ty
  | Number of float
  | String of pstring
  | Int of pstring
  | Float of pstring
  | Binop of bop * expr * expr * ty
  | Call of expr * expr list * expr list

and bop =
| Plus
| Minus | Star | Slash | Eqeq
| EQeqeq of ty  (* We want to carry what type are being compared *)
| Starstar
| Diff | Diff2 | AMpamp | BArbar | Lt
| Lte | Gt | Gte | Dot | Amp | Bar | Ltlt
| Gtgt | Percent | Xor
| Eq of bop option

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

let string_of_ty ty = match ty with
  | TNumber -> "TNumber"
  | TString -> "TString"
  | TBoolean -> "TBoolean"
  | TUnit -> "TUnit"
  | TArrow (ty_list, ty) -> failwith "string_of_ty: Not implemented"
