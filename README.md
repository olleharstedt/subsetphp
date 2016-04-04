Type-inferred strict subset of PHP with LLVM-bindings.

Because Hacklang is not strict or fast enough.

_Disclaimer: Almost nothing is implemented yet._

Version goals
-------------

- 0.0.1: Make a web-page where you can try out the type-inference.
         Prototype LLVM-bindings.
         Prototype AST with types.
         Prototype OCaml GC bindings
         Type-infer int and string and functions return value.
         Use binary math operations and concatenations.
- 0.1: Benchmark nbody, compare with Java, PHP. 
       Post result on Reddit for feedback (r/php, r/programminglanguages, r/programming)
- 0.2: Benchmark binary tree
       Infer arrays of object posted to functions (structural typing?)
       Rewrite nbody with functions
       Fix for-loop bug (`for ($i = 10; $i < 5; $i += 1)`, should not enter loop)
- 0.3: Call php.net functions - how? E.g. var_dump, mysqli.
       Make a static home-page using subsetphp, compare with PHP.
- 0.4: Make a dynamic home-page?
- 0.4: Do all (or some?) "99 problems" in subsetphp (details here: https://www.reddit.com/r/ProgrammingLanguages/comments/16gtqg/say_you_are_trying_to_design_a_programming/)
- 1.0: Convert something (part of a framework?) to subsetphp and run it, benchmark.

TODO: Make up a working solution for optional arguments.

TODO: Better error messages using Pos.

TODO: Parametric polymorphism.

TODO: Compare benchmarks with phc PHP compiler

Notes about type-system
-----------------------

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

Tainted variables: https://wiki.php.net/rfc/taint

Something similar for XSS?

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

Forbid any non-prepared statements to database. But you can concat strings? Like `"SELECT " . $a . " FROM " . $b`. Mark as "dirty" string that cannot be used in query? Tainted, see above.

In OCaml you sometimes want to add type-hints to improve the error messages from the type-checker. No way to do this in subsetphp? Can statically check phpdoc annotations? But not enforce them.

LLVM-compiled and FastCGI? How? API to let user decide what to run only once, when the process starts, like config setup? As in OCaml.

Arrays that are modified should be "promoted" to dynamic arrays internally. Otherwise, static arrays.

LLVM
----

Motivated? HHVM will add some LLVM-support - why should I?

How to represent the IR, without breaking parser/lexer from Hack?

What PHP-features _cannot_ be used from LLVM? Reflection? Add own meta-data (how is it done in Java/C++?).

Hack vs strict Hack vs LLVM. No benchmark for strict Hack?

Call PHP-functions from LLVM? Like `var_dump` etc. Won't work because of different memory model (refcount vs mark-and-sweep). But the compiler will be meaningless without it.

> 17:07:25 - Drup: php -> ast -> typed ast -> IR -> llvm
>
> 17:07:57 - ollehar: why do I need IR in that equation?
>
> 17:08:31 - ggole: You might want to do your own transformations
>
> 17:08:46 - Drup: because you can't write custom optimisations on the typed ast and writing optimisation on the llvm one is quite painful

In php.net, `zend_array` is hash table. But I want to be able to optimize one-dimensional arrays to proper arrays. Must I convert before using php.net functions? To `zend_value`. Either that or rewrite functions that deal with arrays.

> DaveRandom: Ahh I see what you're getting at... yeh it's a tricky one this, you will spend a lot of time simply battling leaky abstractions in php-src. A lot of PHP functions (as in the routines defined using the PHP_FUNCTION macro) are just thin wrappers over a cleaner C API, but some are not.
You'd probably want a way to introduce per-function handlers so that you have a way to shortcut to the back-end, but default to just invoking the zif_* routines defined by PHP_FUNCTION

Related stackoverflow questions:

http://stackoverflow.com/questions/4567195/is-it-possible-to-call-phps-c-functions-in-a-c-program

https://stackoverflow.com/questions/13592037/php-extension-call-existing-php-function

No parallellism. So can never really compete with faster languages. But Unix fork?

The only big problem for communication between a runtime adapted for dynamic vs static language is the arrays. Because arrays in PHP are always HashTable, but in subsetphp I want them to be int[], char[][] etc. Howto solve this? If I want to add new data-structures, I can't use existing functions for e.g. sorting arrays without translating the data-structure to something else.

Be able to turn on/off usage of custom runtime data-structures during compilation?

Are immutable strings faster than mutable?

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

Garbage collection
------------------

* Assume real-time performance is not important
* Assume it's OK to waste memory for better through-put
* Do _not_ assume scripts will run shortly - they may run for days.
* We do not need eager collection, but this should configurable

Features we need or not:

* Generational - yes
* Incremental - no, since real-time speed doesn't matter
* Copying/compacting - probably, to avoid fragmentation

Which one is fastest? Which one is more appropriate for PHP? Need to change semantics of `__destruct` if only GC is used. Only worth it if much faster. Must try both and benchmark?

> It looks into possibilities such as having some types ref counted and not others (IRefCounted!), or having specific instances ref counted, and why none of these solutions were deemed acceptable.

Tune GC to waste memory for higher throughput.

Reference counting or garbage collecting? Must use refcount because of destructors. Remove destructors and deterministic free.

Dead simple GC using shadow-stack and malloc list with mark-sweep.

How to represent GC type-information during runtime?

```llvm
@gc.info.type1 = global {i32, i32} {i32 1, i32 2}
@gc.info.type1_ptr = global i8* bitcast ({i32, i32}* @gc.info.type1 to i8*)
@gc.info.type2 = global {i32, i32, i32} {i32 1, i32 2, i32 3}
@gc.info.type2_ptr = global i8* bitcast ({i32, i32, i32}* @gc.info.type2 to i8*)
@gc.info.types = global [2 x i8** ] [ i8** @gc.info.type1_ptr, i8** @gc.info.type2_ptr ]
```


### Escape analysis

Escape analysis - decide what can be stack allocated and what must be heap allocated during compile time. LLVM has some support if malloc has the right sig?

> LLVM already removes malloc+(perhaps compare with null)+free.

Take a look at:

    Instruction *InstCombiner::visitMalloc(Instruction &MI) {
      // If we have a malloc call which is only used in any amount of comparisons
      // to null and free calls, delete the calls and replace the comparisons with
      // true or false as appropriate.
      if (IsOnlyNullComparedAndFreed(MI)) {

HeapToStack promotion pass?

### Resources

Some resources on why GC can be preferred over refcount (C#, D):

http://blogs.msdn.com/b/brada/archive/2005/02/11/371015.aspx

http://dlang.org/garbage.html

### Discussion about runtime type information

> 19:19:30 - ollehar: hm, is there a conventional way of storing object/struct meta-data for the garbage collector?
>
> 19:21:54 - jonaslund [~chatzilla@92-244-17-198.customers.ownit.se] entered the room.
>
> 19:22:44 - DKordic: ollehar: I don't understand the question.  Can You please elaborate?  
>
> 19:23:03 - ollehar: DKordic: hm, the gc need type information during runtime to know how to collect, right?
>
> 19:24:00 - DKordic: So what should be the ``GC Meta Data'' struct?  In that case choose some GC implementation.  
>
> 19:24:20 - ollehar: found this: http://stackoverflow.com/questions/222117/garbage-collection-and-runtime-type-information
>
> 19:24:21 - ollehar: hm
>
> 19:24:46 - doomlord [~textual@host81-155-67-16.range81-155.btcentralplus.com] entered the room.
>
> 19:25:03 - ollehar: DKordic: yeah, I'm making my own gc at the moment. :P
>
> 19:27:21 - velco: it basically needs to know where pointers are in a type layout
>
> 19:27:31 - velco: say, a sequence of offsets

### Discussions on OCaml IRC

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

Also array copy-on-write: http://hhvm.com/blog/431/on-garbage-collection

Discussion about CoW:

> 15:20:06 - ollehar: use-case is PHP arrays, which have copy on write semantics
>
> 15:20:56 - gasche: you mean they have a value semantics, implemented with CoW?
>
> 15:21:01 - Enjolras: is it ? does it copy the whole array or is it a map under it ?
>
> 15:22:54 - Enjolras: because, if it is a persistant array implemented as a map, the slowdown is for every access. If it copies the whole array, it is linear in the number of write
>
> 15:23:31 - gasche: I think you could amortize the copy cost of persistent array
>
> 15:24:02 - gasche: use a "array plus diff" representation, reading is O(diff size)
>
> 15:24:41 - gasche: sum the diff size over all accesses, and whenever you are larger than the array size, renormalize the array by doing a copy
>
> 15:24:43 - Enjolras: right. That's how virtual memeory is implemented in kernel 
>
> 15:25:31 - gasche: I think your point was that the interaction of "copy on write" and "ref counting" is to know that you can skip the copy when there is a unique owner, but that is only an optimization
>
> 15:25:43 - gasche: (not central to CoW, or in fact value semantics in general)
>
> 15:26:28 - ggole: In the case of arrays, it seems like a pretty necessary optimisation
>
> 15:26:57 - gasche: well my amortization suggestion would suggest that it is only a constant-factor optimisation
>
> 15:27:08 - gasche: and it probably depends a lot on the read/write workflow
>
> 15:27:37 - Enjolras: i have troubles seing the difference between value semantic and cow. It's just two names of the same thing in different contexts, isn't it ?
>
> 15:28:06 - ggole: Wouldn't an update to every element in an array would result in unacceptable (O(n) instead of O(1)) access times to elements?
>
> 15:28:32 - gasche: I understand "value semantics" as a specification (referential transparency, if you want) and CoW as a specific implementation technique to implement unique ownership (share aliased mutable values, unshare on mutation)
>
> 15:28:43 - Enjolras: ggole: not if you use diffs as gaschee explained
>
> 15:29:22 - gasche: there is another trick you can play with persistent arrays, which is to have the last writer to the array always see a plain array, and grow the diff "backward" for other owners
>
> 15:29:30 - gasche: (this is how Filliatre semi-persistent arrays work, for example)
>
> 15:29:47 - ggole: That's what I was assuming, but perhaps it can be done more efficiently than I had in mind
>
> 15:29:49 - gasche: I guess if you do this you can get pretty close to the constant factors of CoW
>
> 15:30:36 - gasche: ggole: my suggestion is to count how much you paid on diff-traversal on the past array accesses, and renormalize the array whenever you reach the array size
>
> 15:30:48 - gasche: hm
>
> 15:31:06 - gasche: actually that means you pay O(n) copy cost every O(sqrt n) operations, so not great
>
> 15:31:28 - govg has left the room (Quit: Ping timeout: 255 seconds).
>
> 15:33:20 - Enjolras: how do you store diffs ? especially sparse ones. If you have a list of arrays, with option, the memory cost is high for sparse diffs. If you have a tree of sequential diffs, your need some code to compact the diffs
>
> 15:33:34 - Enjolras: the complexity of this is not 100% clear to me
>
> 15:35:42 - gasche: https://www.lri.fr/~filliatr/ftp/ocaml/ds/parray.ml.html
>
> 15:36:02 - NingaLeaf [~NingaLeaf@50-193-152-69-static.hfc.comcastbusiness.net] entered the room.
>
> 15:36:25 - gasche: in particular, this implementation has optimal properties for many typical CoW workflows
>
> 15:36:43 - gasche: many owners can share a reference to an array value
>
> 15:37:40 - gasche: the last person to read or write pay an O(1) cost by reading or writing again
>
> 15:38:14 - gasche: if someone that is not the last accessor tries to read or write, a copy happens
>
> 15:39:07 - gasche: if you assume a typical "transfer of ownership" pattern with agents passing the value around, each doing a bunch of read/writes then never touching it again, you have O(1) all the way
>
> 15:39:43 - gasche: (but note that, with this implementation, the copy is O(diff size), not O(array size), which may be costly if a very very old owner resumes activity)
>
> 15:41:57 - gasche: and this is not reference counting; in particular, an old owner can keep a reference of the value as long as it wants, without incurring any cost at all if the reference is not used (which may be very hard to prove statically, and in particular wrong in some exceptional cases)

More discussion about gc and runtime interaction with the compiler:

> 21:49:03 - bitbckt: ollehar: as a first cut, I think it would be just fine to write a simple freelist mark/sweep collector.
>
> 21:49:31 - bitbckt: that should be straightforward enough to give you something to work with, while you build out the remainder of your project.
>
> 21:49:53 - ollehar: bitbckt: this dude did that: http://journal.stuffwithstuff.com/2013/12/08/babys-first-garbage-collector/
>
> 21:50:02 - bitbckt: it won't be competitive by any means, but it's easy and quick to get up and running with.
>
> 21:50:43 - bitbckt: yeah, that's about the gist of it.
>
> 21:51:14 - ollehar: but the roots are explicit in his code
>
> 21:51:22 - ollehar: or the "stack"
>
> 21:51:38 - bitbckt: sure.
>
> 21:51:54 - ollehar: but when you compile to binary without a jit...?
>
> 21:52:16 - bitbckt: a shadow stack is fairly easy to maintain.
>
> 21:52:29 - ollehar: right, llvm has support for that.
>
> 21:52:30 - bitbckt: again, as a first cut.
>
> 21:52:48 - ollehar: sure, I'm happy to get anything up a running ^^
>
> 21:52:58 - ollehar: what's beyond shadow-stack?
>
> 21:54:06 - bitbckt: that usually falls out of the runtime's execution model.
>
> 21:54:43 - bitbckt: threads and their stacks, with stack maps, runtime roots, and foreign handles.
>
> 21:55:02 - ollehar: is this related to the program-counter in any way?
>
> 21:55:10 - ollehar: is that the level we talk about?
>
> 21:55:34 - bitbckt: not the PC necessarily, but there's usually a frame pointer around to traverse the stack.
>
> 21:55:58 - ollehar: does the compiler inserts frame pointers?
>
> 21:56:04 - bitbckt: if not, then there might be a map of PC -> frame descriptor -> stack map.
>
> 21:56:12 - bitbckt: yes.
>
> 21:56:34 - ollehar: so, all this is in the ocaml compiler too, right?
>
> 21:57:12 - bitbckt: presumably, but I'm not an ocaml internals expert.
>
> 21:57:29 - bitbckt: just a lowly language user, in this community :)
>
> 21:59:30 - ollehar: bitbckt: oh btw, just a quick one: what kind of gc do you think would be appropriate for a server environment?
>
> 21:59:44 - ollehar: I know java has a bunch to chose from ^^
>
> 22:00:06 - slash^ has left the room (Quit: Read error: Connection reset by peer).
>
> 22:00:47 - igitoor [igitur@2a00:d880:3:1::c1ca:a648] entered the room.
>
> 22:00:56 - bitbckt: depends upon the latency/throughput trade-offs you'd like to make - variants on mark/sweep generally offer lower latency, while mark/compact generally offers higher throughput.
>
> 22:01:05 - bitbckt: in broad strokes

Strings
-------

String interning, have a pool of all unique strings. Then compare a string is simply a pointer comparison.

The Zend string representation:

```c
struct _zend_string {
	zend_refcounted   gc;
	zend_ulong        h;                /* hash value */
	size_t            len;
	char              val[1];           /* "struct hack", variable sized struct */
};
```

subsetphp shouldn't differ too much from PHP 5.6 representations, because they will have to interact.

Have a global `zend_string` value, and then copy subsetphp pointers to it when using PHP library functions? Or just use PHP structures and don't care about the memory overhead as long as the speed isn't affected?

String interning at runtime? Use Zend string interning structure? Compared to what? OCaml?

`interned_strings` is a `HashTable`.

Use PHP runtime? Like `interned_strings` combiler global. Memory waste, but easier interaction with runtime libraries. No speed penalty for using PHP runtime representations?

OCaml has slower concat than PHP, but OCaml can use Buffer which is _much_ faster than PHP concat. So what to do? Infer string buffer?

    18:53:33 - dmbaturin: ollehar: Well, maybe it's appropriate to treat it as a buffer if destructive operations are used on it, i.e. assign the buffer type to destructive string functions.
    19:10:43 - mrvn: ollehar: maybe strings could be concated lazyliy (just collect a list a strings) and only copied when the result is used.
    19:11:04 - orbitz: THe thing is, I'm not sure using a Buffer helps at all.  Consider: while(true) { x += y; print(x); }

`$a[5]` , then `$a` promoted to buffer. Also at `$a .= ...`.

Ropes uses tree-structure to increase speed of insertion and deletion. O(log n) to get char at position. Possibly faster when iterating over the hole string.

PHP > OCaml > Java for naiv benchmark concat string 100000 times.

OCaml with Buffer: 0.002s
Java with StringBuffer: 0.068s
PHP: 1.5s
OCaml: 2s
Java: 5s

About string interning and mutability at the same time in old OCaml:

    00:14:06 - apache2: let my_name = "mike" in let your_name = "mike" in my_name.[0] <- 'b' ; Printf.printf "%s != %s" my_name your_name
    00:14:15 - apache2: will print "bike != bike"
    00:14:42 - apache2: because someone figured "ah well all strings that are equal might as well be shared"
    00:14:54 - apache2: and then someone else figured "hey these strings should still be mutable"

Immutable strings are good for concurrency, but that's irrelevant in this case since subsetphp never will support this.

Promote strings during runtime?

    21:15:23 - ollehar: what do you think about this idea: a string will be promoted to a string buffer during runtime if it's big enough.
    21:15:33 - ollehar: and maybe to a rope if it's even bigger.
    21:18:27 - Guest28478 [~john@72.168.144.70] entered the room.
    21:19:08 - ggole: So string accesses involve branches to decide how to access the right storage? :/

Maybe use `/** @var StringBuffer */` for strings.

Arrays
------

```php
$arr = [1, 2, 3];  // array<int>
```

Can we infer dynamic arrays and static-size arrays? So we will really have `int[]` and `array<int>` as difference implementation representations. And `object[]` or `string[]`, etc. So the operator `[]=` would promote the type to dynamic array. And using the array with keys promotes it to a hashtable.

```php
$arr = new Array(10);  // new array of size 10
```

```php
$arr = [];  // Type unknown or unresolved
$arr[] = 10;  // Promote to array<int>, dynamic array
$arr[] = 'foo';  // Type error
```

```php
$arr = [1, 2, 3];  // Fix-size array, int[3]
$arr[] = 10;  // Promote to dynamic array, array<int>
```

When looping an array, items should be fetched in order of insertion.

Fixed-length arrays can be implemented directly in LLVM. Are basically like tuples.

```llvm
%0 = [i32 x 3] [1, 2, 3]
```

or with array of struct:

```llvm
%0 = [%struct._s x 3] [%s1, %s2, %s3]
```

Compile-time error if try to access outside of bounds? Even in loops? No.

```php
$arr = [1, 2, 3];
$x = $arr[4];  // Can check compile-time? GCC does this at -O2.
for ($i = 0; $i < 3; $i += 1) {
  // Run-time check so that $i < length of $arr
  $x = $arr[$i];
}
```

Use C-library for dynamic arrays? Glib. Or struct + malloc?

```llvm
%struct.dynarr = type [i32, i32, i8*]  ; size, used, and pointer to malloc area
```

Need to grow array. 

Can't infer array of objects:

```php
function sumPoints(array $points) {
  // Do stuff with points
}
```

No idea what the array is array _of_. Can't infer from usage because of object hierarchy. If it's used like

```php
$points[0]->x = 10;
$points[1]->y = 20;
```

it could be any class with variables x and y. Even harder with interfaces. So much check with docs, or `instanceof`? No, use static duck-typing like in OCaml, but without type-hints. Construct an object "contract"/minimal requirement of methods and member variables. Will this work with LLVM gep? Only if class name is stored runtime.

Also, can't infer if it's fix-size or dynamic-size array - just assume fixed-size until otherwise/if it's not changed in the function? But do we know the length?

If we have both 2DPoints (x, y) and 3DPoints (x, y, z), it won't be possible to enforce which type to use in a function like:

```php
function drawPoints(array $points) {
  // do stuff with $points[i]->x and $points[i]->y
}
```

Or even worse if you have interface with `drawMe()`, and want a function which draws only points, not circles or squares.

Unions
------

nullable is a specific case of this.

Types that are more than one type must be checked with if-statements and typeof before usage.

Examples:
```php
// Type is array<string, string|int[3]>, where | means union
array(
  'asd' => 'qwe',
  'zxc' => array(1, 2, 3)
);
```

Worse performance? Not allowed in Hack, see this link: http://docs.hhvm.com/manual/en/hack.otherrulesandfeatures.uniontypes.php

Classes and objects
-------------------

vtable and stuff.

Could this be represented as a pure struct? No methods or inheritance. How to enforce type? Allow individual type for each usage? Abort compilation if we can't guess type? Or just leave it since it's not used.

```php
final class TestStruct {
  public $x;
  public $y;
}

$struct1 = new TestStruct();
$struct1->x = 10;
$struct1->y = 'foo';

$arrOfStructs = array($struct1);
```

Benchmark
---------

nbody (on my machine):

* PHP 7.0.0 - 3 min (180 sec)
* subsetphp - 10 sec (18x faster than PHP 7)
* Java - 6 sec

The goal is to compare subsetphp with PHP in benchmark "binary trees" from benchmarkgames.
Actually, all benchmarks should be tested. And then also common programming problems, to see how a solution could look in subsetphp.

Requirements:
* Classes
* Recursive classes, methods
* Output strings, printf
* For-loops
* Tertier operator?
* bit wise operations, <<

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

IDE
---

How would a user know what type an array is? E.g., this

```php
$arr = array(1, 2, 3);
```

would be typed as `array<int>`, but if the variable is used later as a hashmap, the type would change?

```php
$arr = array(1, 2, 3);  // array<int>
$arr['asd'] = 10;  // Error, $arr has type array<int>, here used as array<string, int>
```

```php
$arr = array(10 => 20);  // array<int, int>
```

THe point is, use API from the compiler to get type-information. In Vim: At "hover", show type in bar? Look on how Merlin and Hack do it.

Compiler internals
------------------

Discussion about hierarchy in the type system for promotable types. Int should be promotable to Double, and String to StringBuffer. But not String to Double. How to express that with ADT?

```ml
type ty = Int | String | Double | StrintBuffer | Promotable of ref ty
```

> 22:37:10 - mrvn: ollehar: you should divide types into classes first, arithmetic or text for example. Then int and float as members of arithmetic.

> 22:38:23 - mrvn: you could have type 'a arithmetic and `Int arithmetic. Then divide is [< `Int | `Float ] aithemtic -> [< `Int | `Float ] aithemtic -> [`Float] arithmetic

> 23:02:59 - mrvn: If you have "type t = Int : int -> t | Text : string -> t" then you can't limit a function to just one of them. With phantom types you can.

Alternate syntax
----------------

Possible to use another parser and lexer, but same AST.

Double maintenance?

Still PHP 5.6, just another syntax.

* `fn` instead of `function`
* skip `fn` keyword for class methods - we have `public`, `protected` or `private` instead
* no $ for variables
* `.` to access member variables/methods instead of `->`
* no `;` - use `()` to let an expression span multiple lines
* use if-then-else instead of `? :`
* syntactic sugar for `[1, 2, 3].length`, `[1, 2, 3].slice()` etc instead of `count($arr)`
* still use `::` for static access
* make keywords and classes case-sensitive

```
fn x, y {
  return x + y
}

fn arr : array, p : Point {
  arr[0] = p.x
  arr[1] = p.y
}

fn arr : array, x, y {
}

class Point {
  public x
  public y

  public __construct x y {
    this.x = x
    this.y = y
  }

  public drawMe {
    draw(this.x, this.y)
  }

  public toString {
    return x.toString() . y.toString()
  }

  private calculateSomething a, b {
    this.x += a
    this.y += b
  }

  public static getInstance {
    return new Point()
  }
}

fn drawAPoint p : Point, s : Slice {
  p.drawMe(s)
}

foreach arr as key => val {
}

[1, 2, 3].map(fn x => {
  return x + 1
})

something = if empty(foo) then 1 else 10
```

