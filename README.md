# Scheme

Interpreter for the Scheme programming language. Written as an educational
exercise. Implemented in OCaml.

```scheme
$ ./scheme
Scheme REPL
> (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
= value: banana!
> (gcd 10 15)
= value: 5
> (let ((x (gcd 16 32)))
    (display (string-append "The answer is "
                            (number->string x)))
    (newline))
The answer is 16
= value: ()
```


## Build
Requirements on Linux:
* OCaml (tested using OCaml 4.05.0 on Ubuntu 18.04, but any other OCaml version
  could work)
* GNU Make

Makefile targets:
* `make` - Builds the interpreter.
* `make test` - Runs the unit tests.
* `make clean` - Deletes all the built files.
* To create the executables for manual tests of the lexer and parser,
  run `make lexer_test` and `make parser_test`.


## Usage
* `./scheme` - Runs the REPL.  Uses 'rlwrap' for readline support if available.
* `./scheme <file>` - Interprets a file.


## Implemented Primitives
Non-exhaustive list of defined primitives. Primitives are defined in
`./primitives.ml` and `./src-scheme/primitives.scm`.

* `define`, `lambda`, `if`, `quote`, `cond`, `begin`, `let`
* `#t`, `#f`
* `and`, `or`, `not`
* `+`, `-`, `*`, `/`
* `=`, `<`, `>`, `<=`, `>=`
* `abs`, `quotient`, `remainder`, `zero?`, `positive?`, `negative?`, `min`,
  `max`
* `cons`, `car`, `cdr`, `caar`, `cadr`, `cdar`, `cddr`
* `boolean?`, `number?`, `char?`, `string?`, `list?`, `pair?`, `symbol?`,
  `null?`, `vector?`, `procedure?`
* `eq?`, `eqv?`, `equal?`
* `set!`
* `load`, `error`, `display`, `newline`, `exit`
* `number->string`, `string->number`, `char->string`, `string->char`,
  `string->list`, `list->string`, `symbol->string`, `string->symbol`,
  `list->vector`, `vector->list`
* `list`, `make-list`, `length`, `list-ref`, `reverse`, `append`,
  `map`, `filter`, `foldl`, `foldr`, `for-each`, `member`, `index-of`,
  `assoc`, `remove`
* `make-vector`, `vector-length`, `vector-ref`, `vector-set!`
* `char-alphabetic?`, `char-numeric?`, `char-whitespace?`, `char-upper-case?`,
  `char-lower-case?`, `char-upcase`, `char-downcase`
* `string-length`, `string-append`, `string=?`, `string-ref`
* `delay`, `force`
* `#\newline`, `#\space`, `#\tab`
* `quasiquote`, `unquote`, `unquote-splicing`


## Standards Compliance
The [R7RS-small](https://small.r7rs.org/) standard was used as reference, but
this implementation is not compliant:

* Only a subset of primitives required by the standard have been implemented.
* Some implementations of primitives are simplified (in the sense that they
  may not be able to handle all the kinds of arguments that are required by
  the standard).
* Some important language features have been omitted (e.g. the module system,
  hygienic macros, `call-with-current-continuation`).
* Numbers can overflow and underflow.

With the exception of the crude implementation of an unhygienic macro system
(`define-macro`, which could be considered an implementation detail), this
Scheme interpreter represents an honest effort to implement a subset of
R7RS-small Scheme.

Tail call optimization is implemented.


## License
This project is distributed under the BSD 3-Clause License (see LICENSE).
