<?php

function foo($i) {
  return $i + 10;
}

function bar($i) {
  return foo($i) - 5;
}

$a = foo(10) + bar(20);
printd($a);
