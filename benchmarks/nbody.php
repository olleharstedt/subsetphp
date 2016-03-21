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

$saturn = new Body();
$saturn->x = 8.34336671824457987e+00;
$saturn->y = 4.12479856412430479e+00;
$saturn->z = -4.03523417114321381e-01;
$saturn->vx = -2.76742510726862411e-03 * $DAYS_PER_YEAR;
$saturn->vy = 4.99852801234917238e-03 * $DAYS_PER_YEAR;
$saturn->vz = 2.30417297573763929e-05 * $DAYS_PER_YEAR;
$saturn->mass = 2.85885980666130812e-04 * $SOLAR_MASS;

$uranus = new Body();
$uranus->x = 1.28943695621391310e+01;
$uranus->y = -1.51111514016986312e+01;
$uranus->z = -2.23307578892655734e-01;
$uranus->vx = 2.96460137564761618e-03 * $DAYS_PER_YEAR;
$uranus->vy = 2.37847173959480950e-03 * $DAYS_PER_YEAR;
$uranus->vz = -2.96589568540237556e-05 * $DAYS_PER_YEAR;
$uranus->mass = 4.36624404335156298e-05 * $SOLAR_MASS;

$neptune = new Body();
$neptune->x = 1.53796971148509165e+01;
$neptune->y = -2.59193146099879641e+01;
$neptune->z = 1.79258772950371181e-01;
$neptune->vx = 2.68067772490389322e-03 * $DAYS_PER_YEAR;
$neptune->vy = 1.62824170038242295e-03 * $DAYS_PER_YEAR;
$neptune->vz = -9.51592254519715870e-05 * $DAYS_PER_YEAR;
$neptune->mass = 5.15138902046611451e-05 * $SOLAR_MASS;

$sun = new Body();
$sun->mass = $SOLAR_MASS;

function offsetMomentum(Body $body, $px, $py, $pz) {
  $body->vx = -$px / $SOLAR_MASS;
  $body->vy = -$py / $SOLAR_MASS;
  $body->vz = -$pz / $SOLAR_MASS;
}
