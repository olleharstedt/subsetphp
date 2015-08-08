Type-inferred strict subset of PHP with LLVM-bindings possibly.

Because Hacklang is not strict enough.

Because maybe you can't/don't want to commit to another architecture.

_Disclaimer: Nothing is implemented yet._

Version goals
-------------

- 0.0.1: Make a web-page where you can try out the type-inference.
         Prototype llvm-bindings.
         Prototype AST with types.
         Type-infer int and string and functions return value.
         Use binary math operations and concatenations.
- 0.0.2: Type-infer arrays.
- 0.1.0: Benchmark the first program on benchmarkgames, compare with Java, PHP, Hack (strict), Rust, C.
- 1.0.0: Convert something (framework?) to subsetphp and run it, benchmark.

TODO: Make up a working solution for optional arguments.

TODO: Better error messages using Pos.

TODO: Polymorphism.

Notes
-----

```php
$a = 1.0 + 1;  // OK, only one number type: number. Because lack of type-hints and + works on everything. TODO: int/float might be a performance bottle-neck?
echo 1;  // Error: `echo` expects string (only allow print()?). Or don't allow functions without paranthesis?
function_returning_int();  // Error: Must cast to (void) to use function returning non-void without catching output (like ignore in OCaml)
$a == $b;  // Error: Don't allow == anymore
$obj === $obj2;  // Don't allow === on objects? Because it behaves differently than === on values. Always use $obj.equal($obj2).
"bla bla $a";  // Error: No more magic quotes, use sprintf instead.
'bla bla';  // Error: Use only double-quotations for quotes
$a[0];  // Only works on strings and arrays
@foo();  // Don't allow @?
function foo(int $i) {}  // Actually enforce int here. Same with float/string/array. TODO: Can't, because "got int, expected int". PHP 7?
if (): ... endif;  // Error: Don't allow alternative syntax
$foo[bar];  // Error: Don't allow keys without quotes.
!$foo;  // Only allow ! on bools
10 . "asd";  // Error: Can't concat number with string
$query = "SELECT * FROM table WHERE a = " . $a;  // Dirty string because of concatenation with variable, can't be used in database queries.
```

From IRC:

> 23:52:33 - jwatzman|work: class C {} class D extends C {} function f(C $x) {}
>
> 23:52:37 - jwatzman|work: you can pass a D to function()
>
> 23:53:10 - ollehar: ok, but that's the same thing as explicit coersion in ocaml, no?
>
> 23:53:11 - jwatzman|work: inferring what the type of f() is, in the presence of no annotations, is undecidable in the general case (think classes implementing multiple interfaces)

---

Examples:

```php
// Inferred as int -> int -> int
function foo($a, $b) {
  return $a + $b;
}

foo("hej", 10); // Type error: expected int, got string.
```

```php
$a = "asd";
$b = $a . $a;
```

How to deal with `_POST` etc?

How to deal with autoload? Compile files into type-information so that we don't have to traverse
all files each time we type-check?

```php
function a() {
  b(); // b() inferred as void -> void
}

function b() {} // Check so b() corresponds to already inferred type
```

```php
if (true) {
  $a = 10; // $a inferred as int?
}
else {
  $a = "foo";
}

// What type has $a here? Should init $a = 0 before if?
```

Can't infer this?

```php
function f($cond) {
  if ($cond) {  // $cond inferred as bool
    $x = new A();
  else {
    $x = new B();
  }

  return $x;  // What type has $x? If A and B implements more than one interface? Hack does Unresolved[A, B]. Require phpdoc?

  // Force use of settype, and then read second argument? settype($var, "bool");

}
```

Test for null? How does Hack do the nullable types trick? Flow sensitive type-inference?

```php
function(?int $x) : int {
  if ($x !== null) {
    return $x;
  } else {
    return 42;
  }
}
```

Method chaining.

```php
  $this->foo()->bar();
```

By letting fixed-size array be internally represented as tuples, we can enforce correct usage of the `list` operator:
```php
function f($a) {
  return array($a, $a, $a);
}

list($b, $c) = f(10);  // Error: list expected tuple 'a * 'a, but got 'a * 'a * 'a
```

Enforce case-sensitivity? Forbid `Array()` but allow `array()` etc. More info: `https://wiki.theory.org/YourLanguageSucks#PHP_sucks_because`.

Forbid any non-prepared statements to database. But you can concat strings? Like `"SELECT " . $a . " FROM " . $b`. Mark as "dirty" string that cannot be used in query?

In OCaml you sometimes want to add type-hints to improve the error messages from the type-checker. No way to do this in subsetphp? Can statically check phpdoc annotations? But not enforce them.

LLVM-compiled and FastCGI? How? API to let user decide what to run only once, when the process starts, like config setup? As in OCaml.

LLVM
----

Motivated? HHVM will add some LLVM-support - why should I?

How to represent the IR, without breaking parser/lexer from Hack?

What PHP-features _cannot_ be used from LLVM? Reflection? Add own meta-data (how is it done in Java/C++?).

Hack vs strict Hack vs LLVM. No benchmark for strict Hack?

Call PHP-functions from LLVM? Like `var_dump` etc.

> 17:07:25 - Drup: php -> ast -> typed ast -> IR -> llvm
>
> 17:07:57 - ollehar: why do I need IR in that equation?
>
> 17:08:31 - ggole: You might want to do your own transformations
>
> 17:08:46 - Drup: because you can't write custom optimisations on the typed ast and writing optimisation on the llvm one is quite painful

Reference counting or garbage collecting? Must use refcount because of destructors.

> 14:22:30 - ely-se: you need refcounting if you want PHP programs to behave consistently with the official implementation
>
> 14:23:40 - ely-se: in "function function() { $x = new A(); }; function();" it is guaranteed that the destructor of $x is called before function returns
>
> 14:24:01 - companion_cube: ely-se: does it handle ref loops?
>
> 14:24:09 - ely-se: I think it doesn't.
>
> 14:24:20 - flux: it's php, of course it doesn't :-P *badamtish*
>
> 14:24:46 - flux: it does have weak references, though

In php.net, `zend_array` is hash table. But I want to be able to optimize one-dimensional arrays to proper arrays. Must I convert before using php.net functions? To `zend_value`. Either that or rewrite functions that deal with arrays.

> DaveRandom: Ahh I see what you're getting at... yeh it's a tricky one this, you will spend a lot of time simply battling leaky abstractions in php-src. A lot of PHP functions (as in the routines defined using the PHP_FUNCTION macro) are just thin wrappers over a cleaner C API, but some are not.
You'd probably want a way to introduce per-function handlers so that you have a way to shortcut to the back-end, but default to just invoking the zif_* routines defined by PHP_FUNCTION

Related stackoverflow questions:

http://stackoverflow.com/questions/4567195/is-it-possible-to-call-phps-c-functions-in-a-c-program

https://stackoverflow.com/questions/13592037/php-extension-call-existing-php-function

No parallellism. So can never really compete with faster languages. But Unix fork?

The only big problem for communication between a runtime adapted for dynamic vs static language is the arrays. Because arrays in PHP are always HashTable, but in subsetphp I want them to be int[], char[][] etc. Howto solve this? If I want to add new data-structures, I can't use existing functions for e.g. sorting arrays without translating the data-structure to something else.

Be able to turn on/off usage of custom runtime data-structures during compilation?

Here's the definition of `zval` from PHP source:

```c
typedef union _zend_value {
  zend_long         lval;       /* long value */
  double            dval;       /* double value */
  zend_refcounted  *counted;
  zend_string      *str;
  zend_array       *arr;
  zend_object      *obj;
  zend_resource    *res;
  zend_reference   *ref;
  zend_ast_ref     *ast;
  zval             *zv;
  void             *ptr;
  zend_class_entry *ce;
  zend_function    *func;
  struct {
    ZEND_ENDIAN_LOHI(
      uint32_t w1,
      uint32_t w2)
  } ww;
} zend_value;
```

And the array:

```c
struct _zend_array {
  zend_refcounted   gc;
  union {
    struct {
      ZEND_ENDIAN_LOHI_4(
        zend_uchar    flags,
        zend_uchar    nApplyCount,
        zend_uchar    nIteratorsCount,
        zend_uchar    reserve)
    } v;
    uint32_t flags;
  } u;
  uint32_t          nTableMask;
  Bucket           *arData;
  uint32_t          nNumUsed;
  uint32_t          nNumOfElements;
  uint32_t          nTableSize;
  uint32_t          nInternalPointer;
  zend_long         nNextFreeElement;
  dtor_func_t       pDestructor;
};
```

Where Bucket is:

```c
typedef struct _Bucket {
  zval              val;
  zend_ulong        h;                /* hash value (or numeric index)   */
  zend_string      *key;              /* string key or NULL for numerics */
} Bucket;
```

and

```c
typedef struct _zend_array zend_array;
typedef struct _zend_array HashTable;
```

Garbage collection/reference counting
-------------------------------------

Which one is fastest? Which one is more appropriate for PHP? Need to change semantics of `__destruct` if only GC is used. Only worth it if much faster. Must try both and benchmark?

> It looks into possibilities such as having some types ref counted and not others (IRefCounted!), or having specific instances ref counted, and why none of these solutions were deemed acceptable.

Escape analysis - decide what can be stack allocated and what must be heap allocated during compile time.

Benchmark
---------

The goal is to compare subsetphp with PHP in benchmark "binary trees" from benchmarkgames.

Requirements:
* Classes
* Recursive classes
* Static methods
* Output strings
* For-loops
* Tertier operator?

Here's the Java code for said algorithm:

```java
    /**
     * The Computer Language Benchmarks Game
     * http://benchmarksgame.alioth.debian.org/
     *
     * Loosely based on Jarkko Miettinen's implementation. Requires Java 8.
     *
     * contributed by Heikki Salokanto.
     * modified by Chandra Sekar
     * modified by Mike Krüger
     */

    public class binarytrees {
        public static void main(String[] args) throws Exception {
            int n = args.length > 0 ? Integer.parseInt(args[0]) : 0;
            int minDepth = 4;
            int maxDepth = Math.max(minDepth + 2, n);
            int stretchDepth = maxDepth + 1;
            int check = (TreeNode.create(0, stretchDepth)).check();
            
            System.out.println("stretch tree of depth " + (maxDepth + 1) + "\t check: " + check);

            TreeNode longLivedTree = TreeNode.create(0, maxDepth);
            for (int depth = minDepth; depth <= maxDepth; depth += 2)
            {
               int iterations = 1 << (maxDepth - depth + minDepth);
               check = 0;

               for (int i = 1; i <= iterations; i++)
               {
                    check += (TreeNode.create(i, depth)).check();
                    check += (TreeNode.create(-i, depth)).check();
               }
               System.out.println((iterations << 1) + "\t trees of depth " + depth + "\t check: " + check);
            }

            System.out.println("long lived tree of depth " + maxDepth + "\t check: " + longLivedTree.check());
        }

        static class TreeNode {
            int item;
            TreeNode left, right;

            static TreeNode create(int item, int depth)
            {
                return ChildTreeNodes(item, depth - 1);
            }
             
            static TreeNode ChildTreeNodes(int item, int depth)
            {
                TreeNode node = new TreeNode(item);
                if (depth > 0)
                {
                    node.left = ChildTreeNodes(2 * item - 1, depth - 1);
                    node.right = ChildTreeNodes(2 * item, depth - 1);
                }
                return node;
            }

            TreeNode(int item) {
                this.item = item;
            }

            int check() {
                return left == null ? item : left.check() - right.check() + item;
            }
        }
    }
```
