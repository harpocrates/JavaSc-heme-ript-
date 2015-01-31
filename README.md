## JavaSc(heme|ript)

A minimal 400 line implementation of a simple Scheme dialect, built around existing Javascript types. Included are two REPL (read eval print loop) programs - one browser based and the other Node.js based.


[Try it out in your browser now][1].

[1]: http://rawgit.com/harpocrates/JavaSc-heme-ript-/master/JavaSc%28heme|ript%29_REPL.html "JavaSc(heme|ript)""

## Features

  * _Tail call optimized_: The following piece of code to find the nth fibonnacci number will never produce a stack overflow because the function that is doing all the work, `fib1`, is in tail position.

      ```racket
      (define (fib n)
        (local [(define (fib1 n fn fm)
                  (if (= n 0)
                      fn
                      (fib1 (- n 1) fm (+ fn fm))))]
          (fib1 n 0 1)))

      (fib 100)
      => 354224848179262000000
      ```

   There is no stack overflow from calling a function `(f false)` defined `(define (f x) (f x))`. The REPL will just hang indefinitely.
  * _Macros_: This is borrowed from early LISP style, compared to the Scheme syntax rules. A macro is just a function that is evaluated before all other functions, doesn't evaluate its arguments, and returns an s-expression. For example, an `unless` macro is defined in the standard functions file as

      ```racket
      (define-macro (unless condition body)
        `(if ,condition void ,body))

      (unless true "Hello")
      (unless false "World")
      => "World"
      ```

   The usual quote `'`, quasiquote `` ` ``, unquote `,`, and unquote-splicing `,@` apply.
  * _Currying_: An optional feature to permit partial application of functions (inspired from Haskell). For example, since `*` is a function that accepts two arguments, `(* 2)` is equivalent to `(lambda (n) (* 2 n))`. This makes something like generating a list of powers of two a bit simpler:

      ```racket
      (map (expt 2) '(1 2 3 4 5 6 7 8 9 10))
      => (2 4 8 16 32 64 128 256 512 1024)
      ```

   Note that currying implies that for a function `f` of three arguments `(((f a) b) c)`, `((f a b) c)`, `((f a) b c)`, and `(f a b c)` will evaluate to the same thing.
  * _Error stack trace_: The error message will hopefully be helpful, and the stack trace will tell you where the problem came from. In the following code, the function `map` expects a function and a list as arguments. Instead of the function, it gets the literal `9`.

      ```racket
      (append (list 1 2 3 4) (map 9 (list 5 6 7 8)))
      ```

    produces the error message

      ```
        Error: expected a function, not 9
                in call to ('f ('first 'l))
                in call to ('cons ('f ('first 'l)) ('map 'f ('rest 'l)))
                in call to ('append ('list 1 2 3 4) ('map 9 ('list 5 6 7 8)))
      ```

## Deficiencies and Differences with Scheme

  * s-expressions aren't really s-expressions - just arrays.
  * variadic functions aren't supported (except for `list` and fundamental forms). Otherwise, function-currying would be too complex for the user.
  * fundamental forms `define-syntax`, `let-syntax`, `letrec-syntax`, `syntax-rules` are not defined - they are replaced by `define-macro` similar to the LISP `defmacro`
  * a couple forms like `do` and `case` are implemented partially and `delay` is not implemented.
  * the numerical tower is completely off. Numbers are just JavaScript numbers. Same goes for all other types - there are no vectors, characters, or pairs.

## Example

The fixed-point combinator (aka the Y combinator) for transforming recursive functions into non-recursive ones using first-class and anonymous functions.

```racket
; Y combinator
(define (Y f)
  ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y))))))

; Not quite factorial, almost though
(define (almost_factorial f)
  (lambda (n)
    (if (equal? n 0)
        1
        (* n (f (- n 1))))))

; A full-fledged factorial function without any recursion/loops
(define factorial (Y almost_fact))
```

Calling `(factorial 21)` gives `51090942171709440000`.
