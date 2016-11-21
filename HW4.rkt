#lang plai

(require (for-syntax racket/base) racket/match racket/list racket/string
         (only-in mzlib/string read-from-string-all))

;; build a regexp that matches restricted character expressions, can use only
;; {}s for lists, and limited strings that use '...' (normal racket escapes
;; like \n, and '' for a single ')
(define good-char "(?:[ \t\r\na-zA-Z0-9_{}!?*/<=>:+-]|[.][.][.])")
;; this would make it awkward for students to use \" for strings
;; (define good-string "\"[^\"\\]*(?:\\\\.[^\"\\]*)*\"")
(define good-string "[^\"\\']*(?:''[^\"\\']*)*")
(define expr-re
  (regexp (string-append "^"
                         good-char"*"
                         "(?:'"good-string"'"good-char"*)*"
                         "$")))
(define string-re
  (regexp (string-append "'("good-string")'")))

(define (string->sexpr str)
  (unless (string? str)
    (error 'string->sexpr "expects argument of type <string>"))
    (unless (regexp-match expr-re str)
      (error 'string->sexpr "syntax error (bad contents)"))
    (let ([sexprs (read-from-string-all
                 (regexp-replace*
                  "''" (regexp-replace* string-re str "\"\\1\"") "'"))])
    (if (= 1 (length sexprs))
      (car sexprs)
      (error 'string->sexpr "bad syntax (multiple expressions)"))))

(test/exn (string->sexpr 1) "expects argument of type <string>")
(test/exn (string->sexpr ".") "syntax error (bad contents)")
(test/exn (string->sexpr "{} {}") "bad syntax (multiple expressions)")

;; PWAE abstract syntax trees
(define-type PWAE
  [pooh (poohs (listof PWAE?))];-------it is added to existing WAE
  [num  (nums (listof number?))];------it is changed (from number to listof number)
  [add  (left PWAE?) (right PWAE?)]
  [sub  (left PWAE?) (right PWAE?)]
  [with (name symbol?) (init PWAE?) (body PWAE?)]
  [id   (name symbol?)])


; pooh-parse-sexpr : sexpr -> PWAE
;; to convert s-expressions with 'pooh' into listof PWAEs
;; each element of list goes to 'parse-sexpr', it is changed to PWAE, and the PWAEs compose one list again. 
(define (pooh-parse-sexpr sexp)
  (if (null? (rest sexp)) 
      (cons (parse-sexpr (first sexp)) empty) 
      (cons (parse-sexpr (first sexp)) (pooh-parse-sexpr (rest sexp)))))


; parse-sexpr : sexpr -> PWAE
;; to convert s-expressions into PWAEs
;; if sexp is begun with 'pooh', call 'pooh-parse-sexpr'
(define (parse-sexpr sexp)
  (match sexp
    [(? number?) (num (list sexp))] ;------Just a number has to compose a list.
    [(list 'pooh a ..3) (pooh (pooh-parse-sexpr a))]
    [(list '+ l r) (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (with x (parse-sexpr i) (parse-sexpr b))]
    [(list (? number?) ..1) (num sexp)] ;-----if it is a list that its all elements are number...
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax" sexp)]
    ))


;; parses a string containing a PWAE expression to a PWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))


; pooh-subst : PWAE -> PWAE
;; substitution funtion for pooh PWAE 
;; each element(PWAE) of pooh-poohs goes to 'subst', it is substituted, and the PWAEs compose one list again. 
(define (pooh-subst nums from to)
  (if (null? nums)
      empty
      (cons (subst (first nums) from to) (pooh-subst (rest nums) from to))))


; subst : PWAE -> PWAE
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
;; if variant of PWAE is pooh, call 'pooh-subst'
(define (subst expr from to)
  (type-case PWAE expr
    [pooh (nums) (pooh (pooh-subst nums from to))]
    [num (n)   expr]
    [add (l r) (add (subst l from to) (subst r from to))]
    [sub (l r) (sub (subst l from to) (subst r from to))]
    [id (name) (if (symbol=? name from) (num to) expr)]
    [with (bound-id named-expr bound-body)
          (with bound-id
                (subst named-expr from to)
                (if (symbol=? bound-id from)
                    bound-body
                    (subst bound-body from to)))]))



; plus-all : listof (listof number/number) -> listof number
;; Add up all the elements of given list by using bin-op
(define (plus-all nums)
  (if (= (length (rest nums)) 1) 
      (eval (first nums))
      (bin-op '+ (eval (first nums)) (plus-all (rest nums)))))



; pooh-eval : PWAE(pooh) -> PWAE(num)
;; eval funtion for pooh
;; if last element of pooh-poohs is symbol '+', add (sum of rest pooh-poohs) to first nums by using bin-op
;; if last element of pooh-poohs is symbol '-', subtract (sum of rest pooh-poohs) from first nums by using bin-op
(define (pooh-eval nums)
  (cond
    [(symbol=? '+ (id-name (first (reverse nums)))) (bin-op '+ (eval (first nums)) (plus-all (rest nums)))]
    [(symbol=? '- (id-name (first (reverse nums)))) (bin-op '- (eval (first nums)) (plus-all (rest nums)))]
    ))

; eval : PWAE -> listof number
;; evaluates PWAE expressions by reducing them to numbers
(define (eval expr)
  (type-case PWAE expr
    [pooh (nums) (pooh-eval nums)]
    [num (n) n]
    [add (l r) (bin-op '+ (eval l) (eval r))]
    [sub (l r) (bin-op '- (eval l) (eval r))]
    [with (bound-id named-expr bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval named-expr)))]
    [id (name) (error 'eval "free identifier: ~s" name)]
    )) 

;bin-op : (number number -> number) (listof number or number) (listof number or number) -> (listof number))
;; applies a binary numeric function on all combinations of numbers from
;; the two input lists or numbers, and return the list of all of the results
(define (bin-op op ls rs)
  (define (helper l rs)
    ;; f : number -> number
    (define (f num)
      (cond
        [(symbol=? op '+) (+ l num)]
        [(symbol=? op '-) (- l num)]))
    (map f rs))
  (if (null? ls)
      '() 
      (append (helper (first ls) rs) (bin-op op (rest ls) rs))))


; run : string -> listof number
;; evaluate a PWAE program contained in a string
(define (run str)
  (eval (parse str)))



(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh 1 2 -}") '(-1))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{with {x {with {x 20} {pooh 1 x +}}} {with {y 10} {pooh x y -}}}") '(11))
(test (run "{with {x {pooh 1 2 3 4 5 +}} x}") '(15))
(test (run "{pooh {with {x {pooh {1 2} {3 4} 1 +}} x} 2 3 -}") '(0 1 1 2))
(test (run "{pooh 1 2 3 4 5 +}") '(15))
(test (run "{pooh {1 2 3} {4 5} -}") '(-3 -4 -2 -3 -1 -2))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh 1 2 3 4 +}") '(10))
(test (run "{pooh {3 4} {-4 0} 5 +}") '(4 8 5 9))
(test (run "{pooh 1 2 3 4 -}") '(-8))
(test (run "{pooh {4 1} 1 {5 6 7} -}") '(-2 -3 -4 -5 -6 -7))
(test (run "{+ {pooh 1 {4 9} -3 {2 0 1} +} {- {pooh {3 4} {2} -} 4}}") '(1 2 -1 0 0 1 6 7 4 5 5 6))
(test (run "{pooh 1 {pooh 1 2 -} 3 +}") '(3))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh {2 1} {3 4} +}") '(5 6 4 5))
(test (run "{with {x {1 2}} {pooh x {+ {1 2} 1} -}}") '(-1 -2 0 -1))
(test (run "{with {x {1 2}} {pooh x {pooh {1 2} 1 +} -}}") '(-1 -2 0 -1))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{with {x {with {y {1 -2}} {pooh 1 y 2 -}}} {+ x x}}") '(-4 -1 -1 2))


(test (run "{with {x 3} {pooh {+ 8 5} {- y 4} +}}") 0)
(test (run "{with {x {1 2}} {with {x 3} {+ x 4}}}") '(7))
;; additional tests for complete coverage
(test (run "{with {x 2} {- {+ x x} x}}") 2)
(test/exn (run "{with x = 2 {+ x 3}}") "bad syntax")
(test/exn (run "{bleh}") "bad syntax")