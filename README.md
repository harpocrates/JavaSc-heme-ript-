## JavaSc(heme|ript)

A minimal 400 line implementation of a simple Scheme dialect, built around existing Javascript types. Included are two REPL (read eval print loop) programs - one browser based and the other Node.js based.

Follow this link to try it out now in your browser.

## Features

  * _Tail recursion optimized_: The following piece of code to find the nth fibonnacci number will never produce a stack overflow because the function that is doing all the work, `fib1`, is in tail position.

      ```scheme
      (define (fib n)
        (local [(define (fib1 n fn fm)
                  (if (= n 0)
                      fn
                      (fib1 (- n 1) fm (+ fn fm))))]
          (fib1 n 0 1)))
      ```

   There is no stack overflow from calling a function `(f false)` defined `(define (f x) (f x))`. The REPL will just hang indefinitely.
  * _Macros_: This is borrowed from early LISP style, compared to the Scheme syntax rules. A macro is just a function that is evaluated before all other functions, doesn't evaluate its arguments, and returns an s-expression. For example, an `unless` macro is defined in the standard functions file as

      ```scheme
      (define-macro (unless condition body)
        `(if ,condition void ,body))
      ```

   The usual quote `'`, quasiquote `` ` ``, unquote `,`, and unquote-splicing `,@` apply.
  * _Currying_: An optional feature to permit partial application of functions (inspired from Haskell). For example, since `*` is a function that accepts two arguments, `(* 2)` is equivalent to `(lambda (n) (* 2 n))`. This makes something like generating a list of powers of two a bit simpler:

      ```scheme
      (map (expt 2) '(1 2 3 4 5 6 7 8 9 10))
      ```
  * _Error stack trace_: The error message will hopefully be helpful, and the stack trace will tell you where the problem came from. In the following code, the function `map` expects a functions and a list as arguments.

      ```scheme
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
  * the numerical tower is completely off. Numbers are just JavaScript numbers. Same goes for all other types.

## Example

The fixed-point combinator (aka the Y combinator) for transforming recursive functions into non-recursive ones using first-class and anonymous functions.

    ```scheme
    (define (Y f)
      ((lambda (x) (x x))
          (lambda (x) (f (lambda (y) ((x x) y))))))

    (define (almost_factorial f)
      (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))

    (define factorial (Y almost_fact))

    ```

Calling `(fact 21)` gives `51090942171709440000`.