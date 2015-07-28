Type-inferred strict subset of PHP

Because Hacklang is not strict enough.

Because maybe you can't commit to a new language/architecture.

_Disclaimer: Nothing is implemented yet._

```php
$a = 1.0 + 1; // OK, only one number type: number. Because lack of type-hints and + works on everything.
echo 1; // Error: `echo` expects string (only allow print()?). Or don't allow functions without paranthesis?
function_returning_int(); // Error: Must cast to (void) to use function returning non-void without catching output (like ignore in OCaml)
$a == $b; // Error: Don't allow == anymore
"bla bla $a"; // Error: No more magic quotes, use sprintf instead.
'bla bla'; // Error: Use only double-quotations for quotes
$a[0]; // Only works on strings and arrays
@foo(); // Don't allow @?
function foo(int $i) {} // Actually enforce int here. Same with float/string/array. TODO: Can't, because "got int, expected int".
if (): ... endif; // Error: Don't allow alternative syntax
$foo[bar]; // Error: Don't allow keys without quotes.
!$foo; // Only allow ! on bools
10 . "asd"; // Error: Can't concat number with string
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
