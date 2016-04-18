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
  | TInt
  | TInt64
  | TString
  | TString_literal
  | TZend_string_ptr

  (* struct name and field list *)
  | TStruct of string * ty list

  (* Fixed size array, e.g. int[10] or myStruct[10] 
   * This type is promoted to TDynamicSizeArray if used
   * with $arr[] = $anotherElement 
   * Basically a tuple. *)
  | TFixedSizeArray of int * ty

  (* Array where size is known only at runtime *)
  | TDynamicSizeArray of ty

  | TPtr_ptr  (* i8** *)
  | TPtr  (* i8* *)
  | TCaml_value
  | TBoolean
  | TUnit
  | TUnknown  (* Unknown type? Abort? *)

    (* Weak polymorphism, like in struct when you
     * have to see usage before guessing type.
     * "to be resolved" *)
  | TWeak_poly of (ty option) ref

  (* Type that can be promoted. E.g. static array
   * to dynamic array, or string to string buffer *)
  | TPromotable of (ty option) ref

  | TArrow of ty list * ty  (* function type: e.g. `(int, int) -> int` *)

  (* Wrap type in closure to delay its return, so inference can complete *)
  | Delayed of (unit -> ty)

and id = Pos.t * string
and pstring = Pos.t * string

and program = def list

and def =
  | Stmt of stmt
  | Fun of fun_
  | Struct of struct_

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
  | Array of afield list * ty
  | ArrayFixedSize_get of expr * expr * ty  (* lvar, index, ty of return *)
  | ArrayDynamicSize_get of expr * expr * ty  (* lvar, index, ty of return *)
  | True
  | False
  | Id of id * ty
  | Lvar of id * ty
  | Number of float
  | String of pstring
  | Int of pstring
  | Float of pstring
  | Binop of bop * expr * expr * ty
  | Unop of unop * ty
  | Call of expr * expr list * expr list
  | New of expr * expr list * expr list * ty
  | Obj_get of expr * expr * og_null_flavor * ty

and afield =
  | AFvalue of expr

(* Binary operators *)
and bop =
| Plus
| Minus | Star | Slash | Eqeq
| EQeqeq of ty  (* We want to carry what type are being compared *)
| Starstar
| Diff | Diff2 | AMpamp | BArbar | Lt
| Lte | Gt | Gte | Dot | Amp | Bar | Ltlt
| Gtgt | Percent | Xor
| Eq of bop option

(* Unary operators *)
and unop =
  Uminus of expr

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

and og_null_flavor =
  | OG_nullthrows
  | OG_nullsafe

and struct_ = {
  struct_name : string;
  struct_fields : (string * ty) list;
}
[@@deriving show]

let rec string_of_ty ty = match ty with
  | TNumber -> "TNumber"
  | TInt -> "TInt"
  | TInt64 -> "TInt64"
  | TString -> "TString"
  | TString_literal -> "TString_literal"
  | TZend_string_ptr -> "TZend_string_ptr"
  | TFixedSizeArray (size, ty) -> "TFixedSizeArray " ^ (string_of_int size) ^ ", " ^ (string_of_ty ty) 
  | TDynamicSizeArray ty -> "TDynamicSizeArray " ^ (string_of_ty ty) 
  | TStruct (name, tys) -> "TStruct " ^ name ^ ": [" ^ (List.fold_left (fun a b -> a ^ string_of_ty b) "" tys) ^ "]"
  | TPtr_ptr -> "TPtr_ptr"
  | TPtr -> "TPtr"
  | TCaml_value -> "TCaml_value"
  | TBoolean -> "TBoolean"
  | TUnit -> "TUnit"
  | TUnknown -> "TUnknown"
  | TWeak_poly {contents = t} -> begin match t with
      | None ->
          "TWeak_poly None"
      | Some t ->
          "TWeak_poly " ^ string_of_ty t
      end
  | TPromotable {contents = t} -> begin match t with
      | None ->
          "TPromotable None"
      | Some t ->
          "TPromotable " ^ string_of_ty t
    end
  | TArrow (ty_list, ty) -> failwith "string_of_ty: Not implemented"
