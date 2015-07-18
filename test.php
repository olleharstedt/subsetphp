<?php

$a = 10;
$a = 'asd';

/*
[(Ast.Stmt
    (Ast.Expr
       (<opaque>,
        Ast.Binop ((Ast.Eq None), (<opaque>, (Ast.Lvar (<opaque>, "$a"))),
          (<opaque>, (Ast.Int (<opaque>, "10")))))))]
 */
