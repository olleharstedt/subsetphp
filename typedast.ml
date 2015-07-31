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
  | TUnit

type id = Pos.t * string
type pstring = Pos.t * string

type program = def list

and def =
  | Stmt of stmt

and stmt =
  | Expr of expr

and expr = Pos.t * expr_
and expr_ =
  | Id of id * ty  (* Added type here *)
  | Lvar of id * ty
  | Number of float
