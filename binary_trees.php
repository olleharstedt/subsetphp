<?php

/**
 * Binary trees benchmark for subsetphp
 *
 * @author Olle Härstedt
 * @since 2015-08-15
 */

class TreeNode {

  public $item;
  public $left, $right;

  /**
   * @param int $item
   */
  public function __construct($item) {
    $this->item = $item;
  }

  /**
   * @return int
   */
  public function check() {
    return $this->left === null ? $this->item : $this->left->check() - $this->right->check() + $this->item;
  }
}

/**
 * Recursive function creating tree nodes
 *
 * @param int $item
 * @param int $depth
 * @return TreeNode
 */
function ChildTreeNodes($item, $depth)
{
  $node = new TreeNode($item);
  if ($depth > 0)
  {
    $node->left = ChildTreeNodes(2 * $item - 1, $depth - 1);
    $node->right = ChildTreeNodes(2 * $item, $depth - 1);
  }
  return $node;
}

/**
 * @param int $item
 * @param int $depth
 * @return TreeNode
 */
function create($item, $depth)
{
  return ChildTreeNodes($item, $depth - 1);
}
  
$n = $argc === 2 ? $argv[1] : 1;
$minDepth = 4;
$maxDepth = $minDepth + 2 > $n ? $minDepth + 2 : $n;
$stretchDepth = $maxDepth + 1;
$stretchTree = create(0, $stretchDepth);
$check = $stretchTree->check();

printf("stretch tree of depth " . ($maxDepth + 1) . "\t check: " . $check . "\n");

$longLivedTree = create(0, $maxDepth);
for ($depth = $minDepth; $depth <= $maxDepth; $depth += 2)
{
  $iterations = 1 << ($maxDepth - $depth + $minDepth);
  $check = 0;

  for ($i = 1; $i <= $iterations; $i++)
  {
    $check += (create($i, $depth))->check();
    $check += (create(-$i, $depth))->check();
  }
  
  printf("%d\t trees of depth %d\t check: %d\n", $iterations << 1, $minDepth, $check);
}

printf("long lived tree of depth " . $maxDepth . "\t check: " . $longLivedTree->check() . "\n");
