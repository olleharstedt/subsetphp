<?php

/**
 * nbody problem done in subsetphp
 * Copied from the Java version.
 */

final class Body {
  public $x;
  public $y;
  public $z;
  public $vx;
  public $vy;
  public $vz;
  public $mass;
}

//$solar_bodies = [
//];

$PI = 3.141592653589793;
$SOLAR_MASS = 4 * $PI * $PI;
$DAYS_PER_YEAR = 365.24;

$jupiter = new Body();
$jupiter->x = 4.84143144246472090e+00;
$jupiter->y = -1.16032004402742839e+00;
$jupiter->z = -1.03622044471123109e-01;
$jupiter->vx = 1.66007664274403694e-03 * $DAYS_PER_YEAR;
$jupiter->vy = 7.69901118419740425e-03 * $DAYS_PER_YEAR;
$jupiter->vz = -6.90460016972063023e-05 * $DAYS_PER_YEAR;
$jupiter->mass = 9.54791938424326609e-04 * $SOLAR_MASS;
