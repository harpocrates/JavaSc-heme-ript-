var file =
"(define (> x y) (< y x))"+
"(define (<= x y) (or (< x y) (= x y)))"+
"(define (>= x y) (<= y x))"+
"(define (zero? x) (= x 0))"+
"(define (add1 x) (+ x 1))"+
"(define (sub1 x) (- x 1))"+

"(define false? not)"+
"(define (id x) x)"+

"(define (empty? l) (= 0 (length l)))"+
"(define (cons? l) (and (list? l) (not (empty? l))))"+

";; filter :: (x -> bool) [x] -> [x]"+
"(define (filter p? l)"+
"  (cond [(empty? l) empty]"+
"        [(p? (first l)) (cons (first l) (filter p? (rest l)))]"+
"        [else"+
"          (filter p? (rest l))]))"+
";; member? :: x [x] -> bool"+
"(define (member? x l)"+
"  (cond [(empty? l)          false]"+
"        [(equal? x (first l)) true]"+
"        [else"+
"          (member? x (rest l))]))"+
";; foldl :: (y x -> y) y [x] -> y"+
"(define (foldl f base l)"+
"  (if (empty? l)"+
"      base"+
"      (foldl f (f base (first l)) (rest l))))"+
";; foldl :: (x y -> y) y [x] -> y"+
"(define (foldr f base l)"+
"  (if (empty? l)"+
"      base"+
"      (f (first l) (foldr f base (rest l)))))"+
";; foldl1 :: (y x -> y) y [x] -> y"+
"(define (foldl1 f l)"+
"  (foldl f (first l) (rest l)))"+
";; foldl1 :: (x y -> y) y [x] -> y"+
"(define (foldr1 f l)"+
"  (if (= (length l) 1)"+
"      (first l)"+
"      (f (first l) (foldr1 f (rest l)))))"+
";; for-each :: (x -> _) [x] -> void"+
"(define (for-each f l)"+
"  (if (empty? l)"+
"      void"+
"      (begin (f (first l))"+
"             (for-each f (rest l)))))"+
"(define (build-list n f)"+
"  (local [(define (helper start)"+
"            (if (= start n)"+
"                empty"+
"                (cons (f start) (helper (add1 start)))))]"+
"       (helper 0)))"+
";; map :: (x -> y) [x] -> [y]"+
"(define (map f l)"+
"  (if (empty? l)"+
"      empty"+
"      (cons (f (first l)) (map f (rest l)))))"+
";; flip :: (x y -> z) -> (y x -> z)"+
"(define (flip f) (lambda (x y) (f y x)))"+
";; compose :: (x -> y) (y -> z) -> (x -> z)"+
"(define (compose f g) (lambda (x) (f (g x))))"+
";; second :: [x] -> x"+
"(define (second l)"+
"  (first (rest l)))"+
";; third :: [x] -> x"+
"(define (third l)"+
"  (first (rest (rest l))))"+

";;; MACROS"+

";; EXAMPLE: sum 10+9+...+2+1"+
"; (do ([n   10 (sub1 n) ]"+
";      [sum 0  (+ n sum)])"+
";   ((zero? n) sum))"+
"; >>> 55"+
"(define-macro (do vars term-and-body)"+
"  (local [(define end?      (first term-and-body))"+
"          (define output    (second term-and-body))"+
"          (define params    (map first vars))"+
"          (define init-vals (map second vars))"+
"          (define rec-call  (map third vars))"+
"          (define loop      (gensym))]"+
"    `(local [(define (,loop ,@params)"+
"                (if ,end?"+
"                    ,output"+
"                    (,loop ,@rec-call)))]"+
"        (,loop ,@init-vals))))"+

";; EXAMPLE: evaluates definitions one by one, then evaluates body"+
"; (let* ([x 1]"+
";        [y (+ x 1)])"+
";   (list y x))"+
"; >>> (list 2 1)"+
"(define-macro (let* vars body)"+
"  (local [(define list-defines (map (cons 'define) vars))]"+
"    `(local ,list-defines"+
"        ,body)))"+

";; EXAMPLE: like `let*` but it begins by evaluating all the values before binding them"+
"; (let ([x 5])"+
";     (let ([x 2]"+
";           [y x])"+
";       (list y x)))"+
"; >>> (list 5 2)"+
"(define-macro (let vars body)"+
"  (local [(define params (map first vars))"+
"          (define init-vals (map second vars))]"+
"    `((lambda ,params ,body) ,@init-vals)))"+

";; EXAMPLE: creates an anonymous function accessible within itself using `proc-name`"+
"; (recur fac ([n 10])"+
";     (if (zero? n)"+
";         1"+
";         (* n (fac (sub1 n)))))"+
"; >>> 3628800"+
"(define-macro (recur proc-name vars body)"+
"  (local [(define params (map first vars))"+
"          (define init-vals (map second vars))]"+
"    `(local [(define (,proc-name ,@params)"+
"                ,body)]"+
"        (,proc-name ,@init-vals))))"+

";; EXAMPLE: choose only hot temperatures"+
"; (unless (> 15 temperature) (display \"Phew it's hot in here!\"))"+
"(define-macro (unless condition body)"+
"  `(if ,condition void ,body))"+

";; NON-STANDARD MACROS"+

";; EXAMPLE: chooses the branch"+
"; (switch 2"+
";   ([0 '-]"+
";    [1 '+]"+
";    [2 '/]"+
";    [3 '*]))"+
"; >>> '/"+
"(define-macro (switch exp cases)"+
"  (local [(define branches (map (lambda (case) `((equal? ,(first case) ,exp) ,(second case))) cases))]"+
"    `(cond ,@branches)))"+

";; EXAMPLE: evaluates  1 + 9 * 4 > 7 && 3 ^ (5 / 8) > 1 * (2 - 8 * 9)"+
";                        = 37 > 7 && 1.9870133464215778 > -70"+
";                        = true && true"+
";                        = true"+
"; (infix '(1 + 9 * 4 > 7 && 3 ^ (5 / 8) > 1 * (2 - 8 * 9)))"+
"; >>> true"+
"(define-macro (infix inf-exp)"+
"  (local [(define (operator? op)"+
"            (member? op '(&& || > < = + - * / // % ^)))"+
"          (define (priority op)"+
"            (cond [(member? op '(&& ||))    1]"+
"                  [(member? op '(> < =))    2]"+
"                  [(member? op '(+ -))      3]"+
"                  [(member? op '(* / // %)) 4]"+
"                  [(member? op '(^))        5]))"+
"          (define (operator-code op)"+
"            (cond [(member? op '(> < + - * /)) op]"+
"                  [(equal? op '&&)           'and]"+
"                  [(equal? op '||)            'or]"+
"                  [(equal? op '=)         'equal?]"+
"                  [(equal? op '^)           'expt]"+
"                  [(equal? op '//)      'quotient]"+
"                  [(equal? op '%)      'remainder]))"+
"          (define (pop-term inf-exp operators operands)"+
"            (local [(define op (first operators))"+
"                    (define arg1 (second operands))"+
"                    (define arg2 (first operands))"+
"                    (define new-operands (rest (rest operands)))]"+
"                (inf-to-pre inf-exp"+
"                            (rest operators)"+
"                            (cons `(,(operator-code op) ,arg1 ,arg2) new-operands))))"+
"          (define (inf-to-pre inf-exp operators operands)"+
"            (cond [(and (empty? inf-exp) (empty? operators))  (first operands)]"+
"                  [(empty? inf-exp)       (pop-term inf-exp operators operands)]"+
"                  [(list? (first inf-exp))"+
"                      (inf-to-pre (rest inf-exp)"+
"                                  operators"+
"                                  (cons (inf-to-pre (first inf-exp) empty empty) operands))]"+
"                  [(number? (first inf-exp))"+
"                      (inf-to-pre (rest inf-exp)"+
"                                  operators"+
"                                  (cons (first inf-exp) operands))]"+
"                  [(operator? (first inf-exp))"+
"                      (if (or (empty? operators)"+
"                              (< (priority (first operators)) (priority (first inf-exp))))"+
"                          (inf-to-pre (rest inf-exp)"+
"                                      (cons (first inf-exp) operators)"+
"                                      operands)"+
"                          (pop-term inf-exp operators operands))]))]"+
"       (inf-to-pre (second inf-exp) empty empty)))";

