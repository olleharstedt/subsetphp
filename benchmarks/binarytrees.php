<?php

/**
 * Need to implement:
 *   Infer null - Nullable of ty
 *   GC - trace and mark down the tree
 */

final class TreeNode {
  public $item;
  public $left;
  public $right;
}

$node = new TreeNode();
$node->item = 0;
$node->left = null;
$node->right = null;
