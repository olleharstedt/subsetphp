<?php

function bar() {
  $a = 'asd';
  return $a;
}

function foo($a) {
  return '123'. $a;
}

for ($i = 0; $i < 100000; $i += 1) {
  $b = 'qwe' . foo(bar());
  prints($b);
}

/*
  val1 = subsetphp_string_init("asd", 3, 1);
  val2 = subsetphp_string_init("qwe", 3, 1);

  for (int i = 0; i < 500000; i++) {
    val1 = subsetphp_concat_function(val1, val2);
  }
 */

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
