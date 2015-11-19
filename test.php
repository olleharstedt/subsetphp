<?php

final class Point {
  public $x;
  public $y;
}

for ($i = 0; $i < 10; $i += 1) {
  $a = new Point();
  $a->x = 10;  // Infer int for x field, need tvar? not availeble in Typedast because it should be fully typed at that point
  $a->y = 'asd';
}

/*
function foo($a) {
  return $a . 'asd';
}

function bar($a) {
  return 'qwe' . $a;
}

for ($i = 0; $i < 1000; $i += 1) {
  $a = foo('123') . bar('234');
}

echo (1 + 2);
 */

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
