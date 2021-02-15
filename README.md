# Lime

A Rust/Swift-like modern interpreted programming language, hosted by Rust.

- Serious PEG(Parsing Expression Grammar)-based parser
- Most of the statements are expressions that have values
- Dynamic but strict typing system
- First-class functions with Rust's closure style
- Functional techniques, like currying, composing, and higher-order function [WIP]
- "nil with cause" error handling design and nil safety
- REPL with auto-completer, syntax checker, history recorder included
- ...

```swift
class Name { chn, eng }
class Student { name, age, gpa }

impl Student {
    assoc org = "SJTU";

    assoc is_good = |self| { 
        self.gpa >= 4.29;
    };
    assoc print_name = |self| {
        println("Chinese name:", self.name.chn, 
                "\nEnglish name:", self.name.eng);
    };
    assoc try_make_money = |self| {
        if self.is_good() { 1000000000.0; }
        else { nil.expect("there's no money for you"); }
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

var money? = i.try_make_money();
if var money = money? {
    money = money * 0.7;  // tax :P
    println("Wow! I've made $", money);
} else {
    println("Oh-no! I failed since", money?.cause());
}

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
    - [ ] literal overflow handling
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
- [ ] `nil` and `nil` safety
    - [x] `nil`...
    - [x] ...with cause
    - [x] allow `name?` to hold `nil`
    - [ ] ! universal `nil` check on fields and assocs
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
- [x] error-handling
    - [x] panic
    - [x] backtrace for lime error
    - [x] backtrace for all error
    - [x] recoverable Lime errors
        - [x] nil with cause design
            - [x] is_some, is_nil, cause, expect
        - [x] sugars: if var
    - [ ] ...
- [ ] pass-by-value `struct` object
    - [ ] refactor object clone logic
    - [ ] syntax
    - [ ] ...
- [ ] `enum` type
    - [ ] pattern match
- [ ] string interpolation / runtime format
- [ ] immutable value



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
    - [ ] ...
- [ ] web-assembly build


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
