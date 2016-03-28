<?php

// This code is correct, which can be verified by
// running it in PHP 7.

final class Body {
  public $x; 
}

$jupiter = new Body();

$saturn = new Body();

$uranus = new Body();

$neptune = new Body();
$neptune->x = 1;

print($neptune->x);

$sun = new Body();
$sun->x = 2;

print($neptune->x);
