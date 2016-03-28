<?php

// This code is correct, which can be verified by
// running it in PHP 7.

$PI = 3.141592653589793;
$SOLAR_MASS = 4 * $PI * $PI;
$DAYS_PER_YEAR = 365.24;

final class Body {
  public $x; 
  public $y;
  public $z;
  public $vx;
  public $vy;
  public $vz;
  public $mass;
}

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
$sun->x = 0;
$sun->y = 0;
$sun->z = 0;
$sun->vx = 0;
$sun->vy = 0;
$sun->vz = 0;
$sun->mass = $SOLAR_MASS;

$n = 50000000;  // Should read from command-line

$bodies = [
  $sun,
  $jupiter,
  $saturn,
  $uranus,
  $neptune
];

$nrOfBodies = 5;  // Might want to implement count()...

$px = 0;
$py = 0;
$pz = 0;

for($i = 0; $i < $nrOfBodies; $i += 1) {
  $ibody = $bodies[$i];
  $px += $ibody->vx * $ibody->mass;
  $py += $ibody->vy * $ibody->mass;
  $pz += $ibody->vz * $ibody->mass;
}

// Offset momentum
$sun->vx = -$px / $SOLAR_MASS;
$sun->vy = -$py / $SOLAR_MASS;
$sun->vz = -$pz / $SOLAR_MASS;

// Energy
// TODO: Put in function
$e = 0.0;
for ($i = 0; $i < $nrOfBodies; $i += 1) {
  $iBody = $bodies[$i];
  $e += 0.5 * $iBody->mass *
    ( $iBody->vx * $iBody->vx
      + $iBody->vy * $iBody->vy
      + $iBody->vz * $iBody->vz );

  $start = $i + 1;
  if ($start < $nrOfBodies) {  // TODO: Fix for bug
    for ($j = $start; $j < $nrOfBodies; $j += 1) {
      $jBody = $bodies[$j];
      $dx = $iBody->x - $jBody->x;
      $dy = $iBody->y - $jBody->y;
      $dz = $iBody->z - $jBody->z;

      $distance = sqrt($dx*$dx + $dy*$dy + $dz*$dz);
      $e -= ($iBody->mass * $jBody->mass) / $distance;
    }
  }
}

// TODO: Add printf?
print($e);

// Advance n times
// TODO: Put in function
$dt = 0.01;
for ($k = 0; $k < $n; $k += 1) {
  for ($i = 0; $i < $nrOfBodies; $i += 1) {
    $iBody = $bodies[$i];
    $start = $i + 1;
    if ($start < $nrOfBodies) {  // TODO: Fix for bug
      for ($j = $start; $j < $nrOfBodies; $j += 1) {
        $jBody = $bodies[$j];
        $dx = $iBody->x - $jBody->x;
        $dy = $iBody->y - $jBody->y;
        $dz = $iBody->z - $jBody->z;

        $dSquared = $dx * $dx + $dy * $dy + $dz * $dz;
        $distance = sqrt($dSquared);
        $mag = $dt / ($dSquared * $distance);

        $iBody->vx = $iBody->vx - ($dx * $jBody->mass * $mag);
        $iBody->vy = $iBody->vy - ($dy * $jBody->mass * $mag);
        $iBody->vz = $iBody->vz - ($dz * $jBody->mass * $mag);

        $jBody->vx = $jBody->vx + ($dx * $iBody->mass * $mag);
        $jBody->vy = $jBody->vy + ($dy * $iBody->mass * $mag);
        $jBody->vz = $jBody->vz + ($dz * $iBody->mass * $mag);
      }
    }
  }

  for ($i = 0; $i < $nrOfBodies; $i += 1) {
    $iBody = $bodies[$i];
    // TODO: Should support $iBody-> += ...
    $iBody->x = $iBody->x + ($dt * $iBody->vx);
    $iBody->y = $iBody->y + ($dt * $iBody->vy);
    $iBody->z = $iBody->z + ($dt * $iBody->vz);
  }
}

// Energy
// TODO: Put in function
$e = 0.0;
for ($i = 0; $i < $nrOfBodies; $i += 1) {
  $iBody = $bodies[$i];
  $e += 0.5 * $iBody->mass *
    ( $iBody->vx * $iBody->vx
      + $iBody->vy * $iBody->vy
      + $iBody->vz * $iBody->vz );

  $start = $i + 1;
  if ($start < $nrOfBodies) {  // TODO: Fix for bug
    for ($j = $start; $j < $nrOfBodies; $j += 1) {
      $jBody = $bodies[$j];
      $dx = $iBody->x - $jBody->x;
      $dy = $iBody->y - $jBody->y;
      $dz = $iBody->z - $jBody->z;

      $distance = sqrt($dx*$dx + $dy*$dy + $dz*$dz);
      $e -= ($iBody->mass * $jBody->mass) / $distance;
    }
  }
}

print($e);
