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
cargo run --release -- fib.lox --treewalk
```

On my laptop, this prints a timing of 1755 milliseconds. We can run the same thing in the bytecode interpreter using

```
cargo run --release -- fib.lox
```

On the same laptop, this shows a timing of 401 milliseconds.

Now consider hello_world.lox

```
print "hello world!";
```

We can tokenize this with

```
cargo run --release -- hello_world.lox --show-tokens
```

Which gives output

```
tokens: [
    Token { ty: Print, lexeme: "print", literal: None, line: 1, col: 4},
    Token { ty: String, lexeme: ""hello world!"", literal: Some(Str("hello world!")), line: 1, col: 19},
    Token { ty: Semicolon, lexeme: ";", literal: None, line: 1, col: 20},
    Token { ty: Eof, lexeme: "", literal: None, line: 1, col: 20},
]
```

We can show the AST with

```
cargo run --release -- hello_world.lox --show-ast
```

Which gives

```
AST: [
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
cargo run --release -- hello_world.lox --disassemble
```

Giving

```
============ hello_world.lox ============
------------ constants -----------
0    hello world!

------------ code -----------------
0000   OP_CONSTANT hello world! (idx=0)                   line 1
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
cargo run -- f.lox --debug
    Finished dev [unoptimized + debuginfo] target(s) in 0.02s
     Running `target/debug/crafting-interpreters-rs f.lox --debug`
(loxdb) b 4
inserted breakpoint at line 4
(loxdb) g
reached breakpoint at line 4
(loxdb) list
    1    fun b() { c(); }
    2    fun c() {
    3      c("too", "many");
==> 4    }
    5
    6    a();

    0000   OP_GET_GLOBAL String("c") (idx=0)                  line 4
==> 0001   OP_CONSTANT too (idx=1)                            line 4
    0002   OP_CONSTANT many (idx=2)                           line 4
    0003   OP_CALL 2                                          line 4
    0004   OP_POP                                             line 4
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
