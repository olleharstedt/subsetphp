<?php

$a = 1 + 1;
$a = $a + $a;

/*
function foo($a) { 
  return 10;
}

$b = foo(10);
 */

/*
[(Ast.Stmt
    (Ast.Expr
       (<opaque>,
        Ast.Binop ((Ast.Eq None), (<opaque>, (Ast.Lvar (<opaque>, "$a"))),
          (<opaque>, (Ast.Int (<opaque>, "10")))))))]
 */
