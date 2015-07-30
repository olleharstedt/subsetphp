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

What PHP-features _cannot_ be used from LLVM? Reflection?

Hack vs strict Hack vs LLVM. No benchmark for strict Hack?

Call PHP-functions from LLVM? Like `var_dump` etc.

> 17:07:25 - Drup: php -> ast -> typed ast -> IR -> llvm
>
> 17:07:57 - ollehar: why do I need IR in that equation?
>
> 17:08:31 - ggole: You might want to do your own transformations
>
> 17:08:46 - Drup: because you can't write custom optimisations on the typed ast and writing optimisation on the llvm one is quite painful
