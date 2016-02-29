(**
 * Test with promotable types and GADTs
 *
 * http://stackoverflow.com/questions/35676189/expressing-a-hierarchy-within-a-type-system-with-adt-and-possibly-gadt/35683699#35683699
 *) 

type arith
type text

type (_ , _) lessThan =
  | IntToDouble          : (arith, arith) lessThan
  | StringToStringBuffer : (text, text) lessThan

type _ ty =
  | Int          : arith ty
  | Double       : arith ty
  | String       : text ty
  | StringBuffer : text ty
  | Promotable   : 'a ty * ('a, 'promo) lessThan -> 'promo ty

let promote : type a. a ty -> a ty = function
  | Promotable (Int, IntToDouble) -> Promotable (Double, IntToDouble)
  | Promotable (String, StringToStringBuffer) -> Promotable (StringBuffer, StringToStringBuffer)
  | t -> t


let _ =
  let a = Promotable (Int, IntToDouble) in
  let b = promote a in
  ()
