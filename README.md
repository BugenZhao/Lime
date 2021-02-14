# Lime

A Rust/Swift-like modern interpreted programming language, hosted by Rust.

- Serious PEG(Parsing Expression Grammar)-based parser
- Most of the statements are expressions that have values
- Dynamic but strict typing system
- First-class functions with Rust's closure style
- Functional techniques, like currying, composing, and higher-order function [WIP]
- REPL with auto-completer, syntax checker, history recorder included
- ...

```swift
class Name { chn, eng }
class Student { name, age, gpa }

impl Student {
    assoc org = "SJTU";

    assoc is_good = |self| { 
        if self.name.eng == "bugen" { true; }
        else { self.gpa >= 4.29; }
    };
    assoc print_name = |self| {
        println("Chinese name:", self.name.chn, 
                "\nEnglish name:", self.name.eng);
    };
    assoc get_older = |self| {
        self.age = self.age + 1;
        nil;
    };
}

var i = Student {
    name: Name { chn: "zq", eng: "bugen" },
    age: 20.3 as Int,
    gpa: 1.7,
};

i.print_name();
println("Is", i.name.eng, "a good student? =>", i.is_good());

var rep = |n, fn| { || {
    var i = n;
    while (i = i - 1) >= 0 { fn(); }
}; };

var i_get_older = i.get_older;
var i_get_much_older = rep(10, i_get_older);

i_get_much_older();
assert_eq(i.age, 30);

nil;
```

## REPL Demo

[![asciicast](https://asciinema.org/a/xp5O4UQEfQCDT1ZePhdR219gi.svg)](https://asciinema.org/a/xp5O4UQEfQCDT1ZePhdR219gi)


## Roadmap

### Lime

- [x] peg-based grammar
- [x] a basic calculator
- [x] variable set and get
- [x] primitive types and cast expression
- [x] more binary and unary ops
- [x] `print` & `assert` statements
- [x] block expression and scope
- [ ] control flow
    - [x] `if`
    - [x] `while`
        - [x] default branch
        - [x] continue & break with values
    - [ ] `for`
    - [ ] `for in`
    - [x] make them expressions
- [x] `nil` and `nil` safety
- [x] function types and function call
- [ ] function
    - [x] arity check
    - [x] rust native funcs as built-in functions
        - [x] replace `print` stmt with `print` func
        - [x] I/O: `readln`, `time`
        - [x] `panic` and LimeError
        - [ ] ...
    - [ ] functional
        - [ ] composed
        - [x] partial-applied
        - [ ] ...
    - [x] lime function (closure)
    - [x] `return` statement
- [x] resolver
    - [x] preprocess assert stmt
    - [x] semantic analysis for variable binding
    - [ ] `break` `continue` `return` static analysis
- [x] a small lime-hosted prelude standard library
- [x] add force-cmp and ref-cmp ops
- [ ] object
    - [x] refactor a lot for pass-by-ref types
    - [x] class decl and object construction
    - [x] built-in (deep)copy func
    - [x] get field
    - [x] set field
    - [x] `impl` block
        - [x] class-associated funcs
        - [x] class-associated fields
    - [x] `assoc` func call
    - [ ] anonymous fields
    - [ ] casting
    - [ ] indexing `[]`
    - [ ] ...
- [ ] error-handling
    - [x] panic
    - [x] backtrace
    - [ ] recoverable Lime errors
- [ ] pass-by-value object "struct"
    - [ ] refactor object clone logic
    - [ ] syntax
    - [ ] ...


### Lime REPL

- [x] Read! Eval! Print! Loop!
- [ ] auto-completer
    - [x] global-scoped hints
    - [x] token-based hints
    - [ ] analysis while typing
- [x] syntax checker
    - [x] bracket pair checker
    - [x] lime syntax checker
    - [x] lime semantics analysis
- [ ] highligher
    - [x] bracket pair
    - [ ] lime literal
    - [ ] lime keyword
    - [ ] lime syntax
- [ ] special commands
    - [x] basic `:ls`
    - [x] basic `:help`
    - [ ]


### Lime Standard Library
- [ ] built-in collections
    - [ ] `Vec`
    - [ ] `HashMap`
    - [ ] `HashSet`
    - [ ] string utilities
    - [ ] ...
- [ ] higher-order funcs
    - [ ] `map`
    - [ ] `fold`
    - [ ] ...
- [ ] I/O
    - [x] `print` & `println`
    - [x] `readln`
    - [ ] file r/w