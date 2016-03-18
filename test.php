<?php

final class Point {
  public $x;
}

$y = 10;
$p = new Point();
$p->x = 10;
print($p->x);

//final class Something {
  //public $a;
  //public $str;
  //public $str2;
//}

//final class Square {
  //public $a;
//}

//for ($i = 0; $i < 1; $i += 1) {

  //$b = new Something();
  //$b->a = 1;
  //$b->str = 'asd';
  //$b->str2 = 'asd';
  //$c = new Square();
  //$c->a = 1;
//}

//print($a->x);

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
