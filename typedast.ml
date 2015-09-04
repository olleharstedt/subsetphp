(**
 * AST built up by type-inference, with types.
 * Used to generate LLVM IR.
 *
 * @author Olle Harstedt
 * @since 2015-07-29
 *)

open Printf

type ty =
  | TNumber
  | TString
  | TZend_string_ptr
  | TBoolean
  | TUnit
  | TUnknown  (* Unknown type, but need to be refined *)
  | TArrow of ty list * ty  (* function type: e.g. `(int, int) -> int` *)

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
  | Return of Pos.t * expr option * ty
  | For of expr * expr * expr * block
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
  f_body : block;
}
[@@deriving show]

let string_of_ty ty = match ty with
  | TNumber -> "TNumber"
  | TString -> "TString"
  | TZend_string_ptr -> "TZend_string_ptr"
  | TBoolean -> "TBoolean"
  | TUnit -> "TUnit"
  | TUnknown -> "TUnknown"
  | TArrow (ty_list, ty) -> failwith "string_of_ty: Not implemented"
