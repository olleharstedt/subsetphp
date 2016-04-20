<?php

function foo(array $a) {
  $x = $a[0];
  return $x + 1;
}

$arr = [];
$a = foo($arr);
