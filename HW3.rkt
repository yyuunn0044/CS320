#lang plai

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])

(define (symbol<? a b) 
  (string<? (symbol->string a) (symbol->string b)))




; free-ids : WAE → list-of-sym
; WAE의 free-identifier를 구한다. recursive하게 WAE를 순회하며 free인 것만 리스트에 append하는 방식이다. type num은 숫자이므로 empty를 append 했고, add와 sub는 left side와 right side를 다시 함수가 recursive하게 순회하도록 했다. with의 x,i,b는 i에 포함된 free들과 x를 제외한 body의 symbol들을 append 하도록 했다. 단독으로 남은 id는 list 형태로 만들어 추가시켰다. 순서, 중복 관계없이 sym들을 구하는 free-ids1함수의 결과값을 정렬, 중복 제거 기능이 포함된 함수인 free-ids에 넣어 결과값을 얻는다.

(define (free-ids1 wae)
  (type-case WAE wae
    [num (n) '()]
    [add (l r) (append (free-ids1 l) (free-ids1 r))]
    [sub (l r) (append (free-ids1 l) (free-ids1 r))]
    [with (x i b) (append (free-ids i) (remove* (list x) (free-ids b)))]
    [id (s) (list s)]
   ))

(define (free-ids wae)
  (remove-duplicates (sort (free-ids1 wae) symbol<?)))


; binding-ids : WAE → list-of-sym

; WAE의 binding-identifier를 구한다. recursive하게 WAE를 순회하며 binding인 것만 리스트에 append하는 방식이다. binding id들은 with의 name variant에 존재하므로 recursive하게 WAE를 순회하며 그들을 모두 리스트에 넣었다. type이 num와 id인 것들은 의미가 없으므로 empty를 append 했고, add와 sub는 left side와 right side를 다시 함수가 recursive하게 순회하도록 했다. 순서, 중복 관계없이 sym들을 구하는 binding-ids1함수의 결과값을 정렬, 중복 제거 기능이 포함된 함수인 binding-ids에 넣어 결과값을 얻는다.

(define (binding-ids1 wae)
  (type-case WAE wae
    [num (n) '()]
    [add (l r) (append (binding-ids1 l) (binding-ids1 r))]
    [sub (l r) (append (binding-ids1 l) (binding-ids1 r))]
    [with (x i b) (append (list x) (binding-ids1 i) (binding-ids1 b))]
    [id (s) '()]))


(define (binding-ids wae)
  (remove-duplicates (sort (binding-ids1 wae) symbol<?)))


; bound-ids : WAE → list-of-sym
; WAE의 bound-identifier를 구한다. 전체 symbol들에서 free, binding을 제외한 것이 bound이다. 먼저 recursive하게 WAE를 순회하며 중복 관계없이 모든 symbol(identifier)을 list에 추가시키는 allsyms함수를 만들었다. 또한 A리스트의 각각의 원소들에 대해 동일한 원소를 B리스트에서 찾아 하나씩 제거하는 delete 함수를 만들었다. (A리스트의 원소 중 B리스트에 없는 원소는 없다고 가정). delete 함수를 이용하여 allsyms에서 free-ids1함수의 결과값, binding-ids1함수의 결과값을 빼면 bound-id들이 나온다. bound-ids함수는 delete 함수를 포함한다. 그 뒤 결과를 정렬, 중복 제거한다.


(define (delete lst alls)
  (cond
    [(null? lst) (append alls '())]
    [else (delete (rest lst) (remove (first lst) alls))]))



(define (allsyms wae)
  (type-case WAE wae
    [num (n) '()]
    [add (l r) (append (allsyms l) (allsyms r))]
    [sub (l r) (append (allsyms l) (allsyms r))]
    
    [with (x i b) (append (list x) (allsyms i) (allsyms b))]
    [id (s) (list s)]
   ))

(define (bound-ids wae)
  (remove-duplicates (sort (delete (binding-ids1 wae) (delete (free-ids1 wae) (allsyms wae))) symbol<?)))





(test (free-ids (with 'x (num 10) (with 'x (add (num 7) (with 'y (num 4) (add (id 'y) (num 5)))) (add (id 'u) (sub (num 2) (id 'f)))))) '(f u))
(test (binding-ids (with 'x (num 10) (with 'x (add (num 7) (with 'y (num 4) (add (id 'y) (num 5)))) (add (id 'u) (sub (num 2) (id 'f)))))) '(x y))
(test (bound-ids (with 'x (num 10) (with 'x (add (num 7) (with 'y (num 4) (add (id 'y) (num 5)))) (add (id 'u) (sub (num 2) (id 'f)))))) '(y))

(test (free-ids (with 'x (id 'x) (id 'x))) '(x))
(test (binding-ids (with 'x (id 'x) (id 'x))) '(x))
(test (bound-ids (with 'x (id 'x) (id 'x))) '(x))
