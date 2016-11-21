#lang plai-typed

(define-type TMFAE
  [num (n : number)]
  [bool (b : boolean)]
  [pair (f : TMFAE)
        (s : TMFAE)]
  [add (lhs : TMFAE)
       (rhs : TMFAE)]
  [sub (lhs : TMFAE)
       (rhs : TMFAE)]
  [id (name : symbol)]
  [fun (params : (listof symbol))
       (paramtys : (listof TE))
       (body : TMFAE)]
  [app (fun-expr : TMFAE)
       (arg-exprs : (listof TMFAE))]
  [with (names : (listof symbol))
        (nametys : (listof TE))
        (inits : (listof TMFAE))
        (body : TMFAE)]
  [try1 (try-expr : TMFAE)
        (catch-exprs : TMFAE)]
  [throw]
  [and1 (lhs : TMFAE)
        (rhs : TMFAE)]
  [or1 (lhs : TMFAE)
       (rhs : TMFAE)]
  [not1 (x : TMFAE)]
  [eq (one : TMFAE)
      (two : TMFAE)]
  [ifthenelse (b : TMFAE)
              (one : TMFAE)
              (two : TMFAE)]
  [fst (p : TMFAE)]
  [snd (p : TMFAE)])

(define-type TE
  [numTE]
  [boolTE]
  [crossTE (fst : TE)
           (snd : TE)]
  [arrowTE (params : (listof TE))
           (result : TE)])

(define-type TMFAE-Value
  [numV (n : number)]
  [pairV (f : TMFAE-Value)
         (s : TMFAE-Value)]
  [closureV (params : (listof symbol))
            (body : TMFAE)
            (ds : DefrdSub)]
  [boolV (b : boolean)])

(define-type DefrdSub
  [mtSub]
  [aSub (name : symbol)
        (value : TMFAE-Value)
        (rest : DefrdSub)])

(define-type Type
  [numT]
  [boolT]
  [crossT (f : Type)
          (s : Type)]
  [arrowT (param : (listof Type))
          (result : Type)]
  [anyT])

(define-type TypeEnv
  [mtEnv]
  [aBind (name : symbol)
         (type : Type)
         (rest : TypeEnv)])

;; ----------------------------------------
;; length : list -> number;;--------------------------------------------
(define (length lst)
  (if (empty? lst)
      0
      (+ 1 (length (rest lst)))))


;; makesub : (listof symbol) (listof TMFAE) DefrdSub -> DefrdSub
(define (makeSub names inits ds)
  (if (= (length names) 0)
      ds
      (aSub (first names) (interp (first inits) ds) (makeSub (rest names) (rest inits) ds))))


;; list-aaa : (listof TMFAE) -> boolean
(define (list-aaa lst)
  (if (= (length lst) 0)
      true
      (and (aaa (first lst)) (list-aaa (rest lst)))))

;; aaa : TMFAE -> boolean
(define (aaa tmfae)
  (type-case TMFAE tmfae
    [num (n) true]
    [bool (b) true]
    [pair (f s) (and (aaa f) (aaa s))]
    [add (l r) (and (aaa l) (aaa r))]
    [sub (l r) (and (aaa l) (aaa r))]
    [id (name) true]
    [fun (params param-te body-expr)
         (aaa body-expr)]
    [app (fun-expr arg-exprs)
         (and (aaa fun-expr) (list-aaa arg-exprs))]
    [with (names nametys inits body)
          (and (list-aaa inits) (aaa body))]
    [try1 (try-expr catch-exprs)
          (and (aaa try-expr) (aaa catch-exprs))
          ]
    [throw () false]
    [and1 (lhs rhs)
          (and (aaa lhs) (aaa rhs))]
    [or1 (lhs rhs)
         (and (aaa lhs) (aaa rhs))]
    [not1 (x)
          (aaa x)]
    [eq (one two)
        (and (aaa one) (aaa two))]
    [ifthenelse (b one two)
                (and (and (aaa b) (aaa one)) (aaa two))]
    [fst (p) 
        (aaa p)]
    [snd (p)
        (aaa p)]))


;; interp : TMFAE DefrdSub -> TMFAE-Value
(define (interp tmfae ds)
  (type-case TMFAE tmfae
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [pair (f s) (pairV (interp f ds) (interp s ds))]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (name) (lookup name ds)]
    [fun (params param-te body-expr)
         (closureV params body-expr ds)]
    [app (fun-expr arg-exprs)
         (local [(define fun-val (interp fun-expr ds))
                 (define arg-vals arg-exprs)]
           (interp (closureV-body fun-val)
                   (makeSub (closureV-params fun-val)
                         arg-vals
                         (closureV-ds fun-val))))]
    [with (names nametys inits body)
          (interp body (makeSub names inits ds))]
    [try1 (try-expr catch-exprs)
          (if (aaa try-expr)
              (interp try-expr ds)
              (interp catch-exprs ds)) ;;---------------------------------
          ]
    [throw () (numV 1)];------------------------------
    [and1 (lhs rhs)
          (boolV (and (boolV-b (interp lhs ds)) (boolV-b (interp rhs ds))))]
    [or1 (lhs rhs)
         (boolV (or (boolV-b (interp lhs ds)) (boolV-b (interp rhs ds))))]
    [not1 (x)
          (boolV (not (boolV-b (interp x ds))))]
    [eq (one two)
        (boolV (= (numV-n (interp one ds)) (numV-n (interp two ds))))]
    [ifthenelse (b one two)
                (if (boolV-b (interp b ds)) (interp one ds) (interp two ds))]
    [fst (p) (pairV-f (interp p ds))]
    [snd (p) (pairV-s (interp p ds))]))

;; num-op : (number number -> number) -> (TMFAE-Value TMFAE-Value -> TMFAE-Value)
(define (num-op op x y)
  (numV (op (numV-n x) (numV-n y))))

(define (num+ x y) (num-op + x y))
(define (num- x y) (num-op - x y))

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free variable")]
    [aSub (sub-name num rest-ds)
          (if (symbol=? sub-name name)
              num
              (lookup name rest-ds))]))

;; ----------------------------------------


;; get-type : symbol TypeEnv -> Type
(define (get-type name-to-find env)
  (type-case TypeEnv env
    [mtEnv () (error 'get-type "free variable, so no type")]
    [aBind (name ty rest)
           (if (symbol=? name-to-find name)
               ty
               (get-type name-to-find rest))]))

;; ----------------------------------------

;; parse-type : TE -> Type
(define (parse-type te)
  (type-case TE te
    [numTE () (numT)]
    [boolTE () (boolT)]
    [crossTE (f s) (crossT (parse-type f) (parse-type s))]
    [arrowTE (a b) (arrowT (map parse-type a)
                           (parse-type b))]))

(define (type-error tmfae msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                     
                     (to-string tmfae)
                      (string-append " not "
                                     msg)))))

;; forcombine1 : (listof TE) (listof TMFAE) TypeEnv -> (listof Type)
(define (forcombine1 nametys inits env)
  (list-combine (map parse-type nametys) (map (lambda (x) (typecheck x env)) inits)))

;; forcombine2 : (listof Type) (listof TMFAE) TypeEnv -> (listof Type)
(define (forcombine2 tys args env)
  (list-combine tys (map (lambda (x) (typecheck x env)) args)))

;; makeBind : (listof symbol) (listof Type) TypeEnv -> TypeEnv
(define (makeBind names types env)
  (if (= (length names) 0)
      env 
      (aBind (first names) (first types) (makeBind (rest names) (rest types) env))))

;; list-combine : (listof Type) (listof Type) -> (listof Type)
(define (list-combine arg1 arg2)
  (if (= (length arg1) 0)
      empty
      (cons (combine (first arg1) (first arg2)) (list-combine (rest arg1) (rest arg2)))))
  

;; combine : Type Type -> Type
(define (combine a b)
  (type-case Type a
    [numT () (samsam a b)]
    [boolT () (samsam a b)]
    [crossT (f1 s1) 
            (type-case Type b
              [crossT (f2 s2)
                      (crossT (combine f1 f2) (combine s1 s2))]
              [else (type-error b "not crossT")])]
    [arrowT (arg1 fn1)
            (type-case Type b
              [arrowT (arg2 fn2)
                      (arrowT (list-combine arg1 arg2) (combine fn1 fn2))]
              [else (type-error b "not arrowT")])]
    [anyT () b]))
  
;; samsam : Type Type : Type
(define (samsam a b)  
  (cond
    [(and (equal? a b)) a]
    [(and (equal? a (anyT)) (not (equal? b (anyT)))) b]
    [(and (equal? b (anyT)) (not (equal? a (anyT)))) a]
    [(and (equal? a (anyT)) (equal? b (anyT))) (anyT)]
    [else (type-error a "not same")]))


;; typechecks : (listof TMFAE) TypeEnv : boolean
(define (typechecks args env)
  (if (= 0 (length args))
      true
      (begin
        (typecheck (first args) env)
        (typechecks (rest args) env))))


(define typecheck : (TMFAE TypeEnv -> Type)
  (lambda (tmfae env)
    (type-case TMFAE tmfae
      [num (n) (numT)]
      [bool (b) (boolT)]
      [pair (fst snd) (crossT (typecheck fst env) (typecheck snd env))]
      [add (l r) 
           (local
             [(define v (combine (typecheck l env) (typecheck r env)))]
             (cond
               [(or (equal? v (numT)) (equal? v (anyT))) (numT)]
               [else (type-error v "not num")]))]
      [sub (l r)
           (local
             [(define v (combine (typecheck l env) (typecheck r env)))]
             (cond
               [(or (equal? v (numT)) (equal? v (anyT))) (numT)]
               [else (type-error v "not num")]))] 
      [id (name) (get-type name env)]
      [fun (names te body)
           (local [(define param-types (map parse-type te))]
             (arrowT param-types
                     (typecheck body (makeBind names
                                            param-types
                                            env))))]
      [app (fn arg)
           (type-case Type (typecheck fn env)
             [arrowT (param-type result-type)
                     (begin
                       (forcombine2 param-type arg env)
                       result-type)]
             [anyT () (begin
                        (typechecks arg env)
                        (anyT))]
             [else (type-error fn "function")])]
      [with (names nametys inits body)
            (begin
              (forcombine1 nametys inits env)
              (typecheck body (makeBind names (map parse-type nametys) env)))]
      [try1 (try-expr catch-exprs)
            (local [(define t (typecheck try-expr env))
                    (define c (typecheck catch-exprs env))]
              (combine t c))]
      [throw () (anyT)]
      [and1 (lhs rhs)
            (local [(define v (combine (typecheck lhs env) (typecheck rhs env)))]
              (cond
                [(or (equal? v (boolT)) (equal? v (anyT))) (boolT)]
                [else (type-error v "bool")]))]

      [or1 (lhs rhs)
            (local [(define v (combine (typecheck lhs env) (typecheck rhs env)))]
              (cond
                [(or (equal? v (boolT)) (equal? v (anyT))) (boolT)]
                [else (type-error v "bool")]))]
      [not1 (x)
           (type-case Type (typecheck x env)
              [boolT () (boolT)]
              [else (type-error x "bool")])]
      [eq (one two)
            (local [(define v (combine (typecheck one env) (typecheck two env)))]
              (cond
                [(or (equal? v (numT)) (equal? v (anyT))) (boolT)]
                [else (type-error v "bool")]))]
      [ifthenelse (b one two)
           (local [(define bb (typecheck b env))
                   (define choice (combine (typecheck one env) (typecheck two env)))]
             (cond
               [(or (equal? bb (boolT)) (equal? bb (anyT))) choice]
               [else (type-error b "bool")]))]
      [fst (p) (type-case Type (typecheck p env)
                 [crossT (f s) f]
                 [anyT () (anyT)]
                 [else (type-error p "no type")])]
      [snd (p) (type-case Type (typecheck p env)
                 [crossT (f s) s]
                 [anyT () (anyT)]
                 [else (type-error p "no type")])])))

;; ----------------------------------------

(test (interp (num 10)
              (mtSub))
      (numV 10))
(test (interp (add (num 10) (num 17))
              (mtSub))
      (numV 27))
(test (interp (sub (num 10) (num 7))
              (mtSub))
      (numV 3))
(test (interp (app (fun (list 'x) (list (numTE)) (add (id 'x) (num 12)))
                   (list (add (num 1) (num 17))))
              (mtSub))
      (numV 30))
(test (interp (id 'x)
              (aSub 'x (numV 10) (mtSub)))
      (numV 10))

(test (interp (app (fun (list 'x) (list (numTE))
                        (app (fun (list 'f) (list (arrowTE (list (numTE)) (numTE)))
                                  (add (app (id 'f) (list (num 1)))
                                       (app (fun (list 'x) (list (numTE))
                                                 (app (id 'f)
                                                      (list (num 2))))
                                            (list (num 3)))))
                             (list (fun (list 'y) (list (numTE))
                                  (add (id 'x) (id 'y))))))
                   (list (num 0)))
              (mtSub))
      (numV 3))

(test/exn (interp (id 'x) (mtSub))
          "free variable")

(test (typecheck (num 10) (mtEnv))
      (numT))

(test (typecheck (add (num 10) (num 17)) (mtEnv))
      (numT))
(test (typecheck (sub (num 10) (num 7)) (mtEnv))
      (numT))

(test (typecheck (fun (list 'x) (list (numTE)) (add (id 'x) (num 12))) (mtEnv))
      (arrowT (list (numT)) (numT)))

(test (typecheck (fun (list 'x) (list (numTE)) (fun (list 'y) (list (boolTE)) (id 'x))) (mtEnv))
      (arrowT (list (numT)) (arrowT (list (boolT)) (numT))))

(test (typecheck (app (fun (list 'x) (list (numTE)) (add (id 'x) (num 12)))
                      (list (add (num 1) (num 17))))
                 (mtEnv))
      (numT))

(test (typecheck (app (fun (list 'x) (list (numTE))
                           (app (fun (list 'f) (list (arrowTE (list (numTE)) (numTE)))
                                     (add (app (id 'f) (list (num 1)))
                                          (app (fun (list 'x) (list (numTE)) (app (id 'f) (list (num 2))))
                                               (list (num 3)))))
                                (list (fun (list 'y) (list (numTE))
                                     (add (id 'x)
                                          (id' y))))))
                      (list (num 0)))
                 (mtEnv))
      (numT))

(test/exn (typecheck (app (num 1) (list (num 2))) (mtEnv))
          "no type")

(test/exn (typecheck (add (fun (list 'x) (list (numTE)) (num 12))
                          (num 2))
                     (mtEnv))
          "no type")


;----------------------------------------------------------------------

(test (interp (eq (num 13)
                  (ifthenelse (eq (num 1) (add (num -1) (num 2)))
                              (num 12)
                              (num 13)))
              (mtSub))
      (boolV false))

(test (typecheck (eq (num 13)
                     (ifthenelse (eq (num 1) (add (num -1) (num 2)))
                                 (num 12)
                                 (num 13)))
                 (mtEnv))
      (boolT))
(test/exn (typecheck (add (num 1)
                          (ifthenelse (bool true)
                                      (bool true)
                                      (bool false)))
                     (mtEnv))
          "no type")

(test (interp (fst (pair (num 10) (num 8))) (mtSub)) (numV 10))
(test (interp (snd (pair (num 10) (num 8))) (mtSub)) (numV 8))
(test (typecheck (pair (num 10) (num 8)) (mtEnv)) (crossT (numT) (numT)))
(test (typecheck (add (num 1) (snd (pair (num 10) (num 8)))) (mtEnv)) (numT))
(test (typecheck (fun (list 'x) (list (crossTE (numTE) (boolTE)))
                      (ifthenelse (snd (id 'x)) (num 0) (fst (id 'x))))
                 (mtEnv))
      (arrowT (list (crossT (numT) (boolT))) (numT)))
(test/exn (typecheck (fst (num 10)) (mtEnv)) "no type")
(test/exn (typecheck (add (num 1) (fst (pair (bool false) (num 8)))) (mtEnv)) "no type")
(test/exn (typecheck (fun (list 'x) (list (crossTE (numTE) (boolTE)))
                          (ifthenelse (fst (id 'x)) (num 0) (fst (id 'x))))
                     (mtEnv))
          "no type")


(define run : (TMFAE -> TMFAE-Value)
  (lambda (tmfae)
    (interp tmfae (mtSub))))

(define eval : (TMFAE -> TMFAE-Value)
 (lambda (tmfae)
    (begin
      (try (typecheck tmfae (mtEnv))
           (lambda () (error 'type-error "typecheck")))
      (run tmfae))))

;----------------------------------------------------------------------------------------------

(test (run (app (fun (list) (list) (num 10)) (list))) (numV 10))
(test (run (app (fun (list 'x 'y) (list (numTE) (numTE))
                        (sub (id 'x) (id 'y))) (list (num 10) (num 20))))
      (numV -10))
(test (typecheck (app (fun (list 'x 'y) (list (numTE) (boolTE))
                           (id 'y))
                      (list (num 10) (bool false)))
                 (mtEnv))
      (boolT))
(test/exn (typecheck (app (fun (list 'x 'y) (list (numTE) (boolTE))
                               (id 'y))
                          (list (num 10) (num 10)))
                     (mtEnv))
          "no type")

(test (typecheck (throw) (mtEnv)) (anyT))
(test (typecheck (try1 (num 8) (num 10)) (mtEnv)) (numT))
(test (typecheck (try1 (throw) (num 10)) (mtEnv)) (numT))
(test/exn (typecheck (try1 (num 8) (bool false)) (mtEnv)) "no type")
(test (typecheck (ifthenelse (throw) (num 1) (num 2)) (mtEnv)) (numT))
(test/exn (typecheck (app (throw) (list (ifthenelse (num 1) (num 2) (num 3)))) (mtEnv)) "no type")
(test/exn (typecheck (add (bool true) (throw)) (mtEnv)) "no type")
(test (typecheck (fst (throw)) (mtEnv)) (anyT))
(test (typecheck (ifthenelse (bool true) (pair (num 1) (throw)) (pair (throw) (bool false))) (mtEnv)) (crossT (numT) (boolT)))

(test (typecheck (add (throw) (throw)) (mtEnv)) (numT))
(test (typecheck (app (throw) (list (num 10) (num 10))) (mtEnv)) (anyT))
(test (typecheck (try1 (add (num 1) (throw)) (throw)) (mtEnv)) (numT))
(test (typecheck (ifthenelse (bool false) (num 2) (throw)) (mtEnv)) (numT))
(test (typecheck (ifthenelse (bool false) (throw) (num 2)) (mtEnv)) (numT))
(test (typecheck (ifthenelse (bool false) (throw) (throw)) (mtEnv)) (anyT))
(test (typecheck (pair (num 2) (bool false)) (mtEnv)) (crossT (numT) (boolT)))
(test (typecheck (pair (num 2) (throw)) (mtEnv)) (crossT (numT) (anyT)))
(test (typecheck (snd (throw)) (mtEnv)) (anyT))
(test (typecheck (snd (pair (num 2) (bool false))) (mtEnv)) (boolT))
(test (typecheck (fun empty empty (num 2)) (mtEnv)) (arrowT empty (numT)))
(test (typecheck (fun (list 'x) (list (numTE)) (throw)) (mtEnv)) (arrowT (list (numT)) (anyT)))
(test (typecheck (app (fun empty empty (num 2)) empty) (mtEnv)) (numT))
(test (typecheck (app (throw) (list (num 2) (bool false))) (mtEnv)) (anyT))
(test (typecheck (app (fun (list 'x 'y) (list (numTE) (numTE)) (add (id 'x) (id 'y))) (list (num 2) (num 3))) (mtEnv)) (numT))
(test (typecheck (with (list 'x) (list (numTE)) (list (num 2)) (id 'x)) (mtEnv)) (numT))
(test (typecheck (with (list 'x) (list (numTE)) (list (throw)) (id 'x)) (mtEnv)) (numT))
(test (typecheck (with (list 'x 'y 'z) (list (boolTE) (numTE) (numTE)) (list (bool false) (num 2) (num 3)) (ifthenelse (id 'x) (id 'y) (id 'z))) (mtEnv)) (numT))
(test (typecheck (with empty empty empty (num 2)) (mtEnv)) (numT))
(test (typecheck (with (list 'x) (list (numTE)) (list (throw)) (num 2)) (mtEnv)) (numT))

(test (run (try1 (try1 (pair (throw) (num 5)) (fun (list 'x) (list (numTE)) (add (id 'x) (num 3)))) (pair (num 2) (num 1)))) (pairV (numV 2) (numV 1)))
(test (run (try1 (ifthenelse (eq (num 4) (num 5)) (num 0) (add (num 4) (sub (num 4) (num 5)))) (fun (list 'd) (list (boolTE)) (and1 (bool false) (id 'd))))) (numV 3))