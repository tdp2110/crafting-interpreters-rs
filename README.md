# Crafting Interpreters in Rust

Giving https://craftinginterpreters.com/ a try, while learning Rust at the same time.

~~Just getting started with both :)~~

:crab: :crab: :crab: This now includes two fairly complete implementations of Bob Nystrom's Lox language: one as a tree-walk interpreter, and the other as a bytecode interpreter. The treewalk interpreter does not include a garbage collector (the bytecode interpreter does). The bytecode interpreter is written completely in safe Rust (though an unsafe version would likely be much faster). :crab: :crab: :crab:

## Examples

Consider fib.lox

```
fun fib(n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

var before = clock();
print fib(25);
var after = clock();
print after - before;
```

We can run this in the treewalk interpreter using

```
cargo run --release --quiet -- fib.lox --treewalk
```

On my laptop, this prints a timing of 1755 milliseconds. We can run the same thing in the bytecode interpreter using

```
cargo run --release --quiet -- fib.lox
```

On the same laptop, this shows a timing of 401 milliseconds.
For comparison, on the same laptop, [the tiger compiler](https://github.com/tdp2110/HaskellTiger)
computes the same answer in `0.00s user 0.00s system 4% cpu 0.077 total` (not counting compilation :)). A C compiler,
or tigerc using the llvm backend :), computes this in `0.00s user 0.00s system 65% cpu 0.004 total`.

Now consider hello_world.lox

```
print "hello world!";
```

We can tokenize this with

```
cargo run --release --quiet -- hello_world.lox --show-tokens
```

Which gives output

```
[
    Token { ty: Print, lexeme: "print", literal: None, line: 1, col: 4},
    Token { ty: String, lexeme: ""hello world!"", literal: Some(Str("hello world!")), line: 1, col: 19},
    Token { ty: Semicolon, lexeme: ";", literal: None, line: 1, col: 20},
    Token { ty: Eof, lexeme: "", literal: None, line: 1, col: 20},
]
```

We can show the AST with

```
cargo run --release --quiet -- hello_world.lox --show-ast
```

Which gives

```
[
    Print(
        Literal(
            String(
                "hello world!",
            ),
        ),
    ),
]
```

Finally, we can show compiled bytecode with

```
cargo run --release --quiet -- hello_world.lox --disassemble
```

Giving

```
============ hello_world.lox ============
------------ constants -----------
0    "hello world!"

------------ code -----------------
0000   OP_CONSTANT "hello world!" (idx=0)                 line 1
0001   OP_PRINT                                           line 1
0002   OP_NIL                                             line 1
0003   OP_RETURN                                          line 1
```

## Debugger

This project includes a basic (in-progress, possibly never to progress further) debugger.

For example, consider f.lox

```
fun a() { b(); }
fun b() { c(); }
fun c() {
  c("too", "many");
}

a();
```

We can explore this in the debugger with

```
$ cargo run --release --quiet -- f.lox --debug
(loxdb) b 4
inserted breakpoint at line 4
(loxdb) g
reached breakpoint at line 4
(loxdb) list
    2    fun b() { c(); }
    3    fun c() {
==> 4      c("too", "many");
    5    }
    6
    7    a();

==> 0000   OP_GET_GLOBAL String("c") (idx=0)                  line 4
    0001   OP_CONSTANT "too" (idx=1)                          line 4
    0002   OP_CONSTANT "many" (idx=2)                         line 4
    0003   OP_CALL 2                                          line 4
(loxdb) bt
[line 7] in script
[line 1] in a()
[line 2] in b()
[line 4] in c()
(loxdb) g
Lox runtime error: Expected 0 arguments but found 2..

Traceback:

[line 7] in script
[line 1] in a()
[line 2] in b()
[line 4] in c()
```

## REPL

A REPL for interactive development is also available, which uses the slower treewalk interpreter. Launch with

```
cargo run --release --quiet
```

Here's an example session:

```
$ cargo run --release --quiet
============================================
Welcome to lox! using tree-walk interpreter.
============================================

>>> var x = 42;
>>> fun f(n) { return n + 1; }
>>> f(x);
43
```

## Extensions

Using the `--Xlists` command line switch, we can enable lists

```
===================================================
Welcome to lox 0.1.0! Using tree-walk interpreter.

Authors: Thomas Peters <thomas.d.peters@gmail.com>
===================================================

>>> var xs = [1,2,3]
>>> xs
[1, 2, 3]
```

Lists don't have much functionality yet, but they have lengths

```
>>> len(xs)
3
```

can be concatenated 

```
>>> var ys = xs + xs
>>> ys
[1, 2, 3, 1, 2, 3]
```

can be mapped over

```
>>> fun square(x) { return x * x; }
>>> map(square, xs)
[1, 4, 9]
>>>
```

can be iterated 

```
>>> fun printFun(elt) { print elt; }
>>> forEach(xs, printFun)
1
2
3
```

and also have expected indexing operators

```
>>> xs[0] = -xs[0]
-1
>>> xs
[-1, 2, 3]
```
