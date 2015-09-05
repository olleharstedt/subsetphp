<?php

function foo($str) {
  $result = $str . 'asd';
  return $result;
}

$b = foo('asd');

for ($i = 0; $i < 10; $i += 1) {
  $b = foo($b);
}

print($b);

/* Simple benchmark with some number crunching in a tight loop. Add mult and div?
function foo($i) {
  return $i + 10;
}

function bar($i) {
  return foo($i) - 5;
}

function baz() {
  $a = 0;
  for ($i = 0; $i < 1000000; $i += 1) {
    $a += foo(10) + bar(20);
  }
  return $a;
}

print(baz());
 */
