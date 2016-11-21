#lang plai

; type COURSE: 과목에 관한 타입으로, variant로 CS320, CS311, CS330을 갖고 있으며 각 variant는 field로 quiz, homework, projects등을 가지고 있다. 각 field의 value는 모두 number이다.
(define-type COURSE
  [CS320 (quiz number?)
         (homework number?)]
  [CS311 (homework number?)]
  [CS330 (projects number?)
         (homework number?)])

; have-homework: COURSE -> number
; 주어진 COURSE가 CS320, CS311, CS330일 각각의 경우에 대해 homework field 값을 가져온다.
(define (have-homework a)
  (cond
    [(CS320? a) (CS320-homework a)]
    [(CS311? a) (CS311-homework a)]
    [(CS330? a) (CS330-homework a)]
    )
  )

(test (have-homework(CS320 3 2)) 2)
(test (have-homework(CS311 5)) 5)
(test (have-homework(CS330 6 1)) 1)

(test (have-homework(CS320 3 2)) 3)
(test (have-homework(CS311 5)) 2)
(test (have-homework(CS330 6 17)) 16)

; have-projects: COURSE -> Boolean
; 주어진 COURSE가 CS320이나 CS311인지 먼저 확인하고, 그렇다면 false를 반환한다. 아니라면 CS330이므로 projects field가 2 이상인지 확인해 맞을 떄에만 true를 반환한다.
(define (have-projects a)
  (cond
    [(or (CS320? a) (CS311? a)) false]
    [(>= (CS330-projects a) 2) true]
    [else false]
    )
  )

(test (have-projects(CS320 3 2)) #t)
(test (have-projects(CS311 5)) #t)
(test (have-projects(CS330 1 1)) #t)

(test (have-projects(CS320 3 2)) #t)
(test (have-projects(CS311 5)) #t)
(test (have-projects(CS330 6 17)) #t)

; name-pets: (listof symbol) -> (listof symbol)
; 바꾸고자 하는 원래 list인 names를 받아온다. names의 첫번째 인자를 확인하고 'dog, 'cat, 'pig 중 하나라면 cons를 이용하여 'happy, 'smart, 'pinky 중 맞는 것을 추가시킨다. 아니라면 first를 그대로 추가시킨다. 그 다음 인자를 확인하기 위해 names의 rest 부분을 인자로 주어 name-pets를 recursive하게 call한다. Recursive 문의 종료를 위해 condition 맨 앞 조건으로 names가 null일 때 empty를 반환하도록 한다. 

(define (name-pets names)
  (cond 
    [(eq? names empty) (append empty empty)]
    [(symbol=? 'dog (first names)) (cons 'happy (name-pets (rest names)))]
    [(symbol=? 'cat (first names)) (cons 'smart (name-pets (rest names)))]
    [(symbol=? 'pig (first names)) (cons 'pinky (name-pets (rest names)))]
    [else (cons (first names) (name-pets (rest names)))])
  )

(test (name-pets (list 'dog 'qwer 'cat)) (list 'happy 'qwer 'smart))
(test (name-pets (list 'aaa 'dog 'pig 'cat)) (list 'aaa 'happy 'pinky 'smart))
(test (name-pets '()) '())

; give-name: symbol symbol (listof symbol) -> (listof symbol)
; 바꾸고자 하는 원래 list인 names, 원래이름과 새 이름인 old, new를 받아온다. names의 첫번째 인자를 확인하고 old라면 cons를 이용하여 new를 추가시키고 아니라면 first를 그대로 추가시킨다. 그 다음 인자를 확인하기 위해 names의 rest 부분을 인자로 주어 give-name을 recursive하게 call한다. Recursive 문의 종료를 위해 condition 맨 앞 조건으로 names가 null일 때 empty를 반환하도록 한다. 
(define (give-name old new names)
  (cond 
    [(eq? names empty) (append empty empty)]
    [(symbol=? old (first names)) (cons new (give-name old new (rest names)))]
    [else (cons (first names) (give-name old new (rest names)))])
  )

(test (give-name 'bear 'pooh (list 'aaa 'bear 'pig 'bear)) (list 'aaa 'pooh 'pig 'pooh))
(test (give-name 'panda 'cutie (list 'panda 'aaa 'pig 'bear)) (list 'cutie 'aaa 'pig 'bear))