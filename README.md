Type-inferred strict subset of PHP

Because Hacklang is not strict enough.

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

How to deal with autoload?

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
