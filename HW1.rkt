#lang plai


; yen->won: number -> number
; 옌 단위의 금액을 받아서 원 단위로 환산한 금액을 원래의 금액에 10을 곱해 계산한다.

(define (yen->won yen)
  (* yen 10))
(test (yen->won 10) 100)

; is-multiple-eleven?: number -> boolean
; modulo 함수를 이용하여 특정 수가 11의 배수인지 아닌지 판별한다. 11의 배수이면 #t, 아니면 #f를 리턴한다. 
(define (is-multiple-eleven? a)
  (cond
  [(= (modulo a 11) 0) true]
  [else false]))
(test (is-multiple-eleven? 121) true)
(test (is-multiple-eleven? 131) false)

; area-triangle: number number -> number
; 삼각형의 높이, 밑변의 길이를 받아 넓이를 계산한다. (0.5*밑변*높이)

(define (area-triangle base height)
  (/ (* base height) 2))
(test (area-triangle 10 5) 25)

; max: number number number number number -> number
; 다섯 개의 수를 받아 그 중 가장 큰 수를 출력한다. 계산을 편리하게 하기 위해 두 수의 대소를 판별해 큰 수를 반환하는 max1 함수를 추가하였다.
(define (max1 a b)
  (cond
    [(>= a b) a]
    [else b]))
(define (max a b c d e)
  (max1 (max1 (max1 (max1 a b) c) d) e))
(test (max 3 1 4 5 2) 5)

; min: number number number number number -> number
; 다섯 개의 수를 받아 그 중 가장 작은 수를 출력한다. 계산을 편리하게 하기 위해 두 수의 대소를 판별해 작은 수를 반환하는 min1 함수를 추가하였다.

(define (min1 a b)
  (cond
    [(>= a b) b]
    [else a]))
(define (min a b c d e)
  (min1 (min1 (min1 (min1 a b) c) d) e))
(test (min 3 1 4 5 2) 1)
