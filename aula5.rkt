#lang racket

(define (soma-lista lst)
  (if (empty? lst) 0
      (+ (first lst) (soma-lista (rest lst)))))

#|

Pelo modelo de substituicao:

(soma-lista (list 3 5 2))
 = (+ 3 (soma-lista '(5 2)))
 = (+ 3 (+ 5 (soma-lista '(2))))
 = (+ 3 (+ 5 (+ 2 (soma-lista '()))))
 = (+ 3 (+ 5 (+ 2 0)))
 = (+ 3 (+ 5 2))
 = (+ 3 7)
 = 10

|#

(define (pertence? x lst)
  (cond [(empty? lst) #f]
        [(equal? x (first lst)) #t]
        [else (pertence? x (rest lst))]))

#|

(pertence? 3 (lista 1 4 5 7))
 = (pertence? 3 '(4 5 7))
 = (pertence? 3 '(5 7))
 = (pertence? 3 '(7))
 = (pertence? 3 '())
 = #f


|#



(define (soma-lista-2 lst)
  (define (soma-lista-acc lst acc)
    (if (empty? lst)
        acc
        (soma-lista-acc (rest lst) (+ (first lst) acc))))
  (soma-lista-acc lst 0))

#|

(soma-lista-acc '(3 5 2) 0)
 = (soma-lista-acc '(5 2) (+ 3 0))
 = (soma-lista-acc '(5 2) 3)
 = (soma-lista-acc '(2) (+ 5 3))
 = (soma-lista-acc '(2) 8)
 = (soma-lista-acc '() (+ 2 8))
 = (soma-lista-acc '() 10)
 = 10


|#


;; --- Exercício 2.1 ---------------------

;; Crie uma função (mult m n) que multiplica os dois números
;; naturais m e n, usando apenas a operação de soma.

(define (mult n m)
  (define (mult n m current)
    (if (= m 0) current
        (mult n (sub1 m) (+ n current))))
  (mult n m 0))

(define-test-suite testes-mult
  (test-equal? "3 * 4"  (mult 3 4)    12)
  (test-equal? "5 * 0"  (mult 5 0)    0)
  (test-equal? "0 * 5"  (mult 0 5)    0)
  (test-equal? "13 * 1" (mult 13 1)   13)
  (test-equal? "1 * 13" (mult 1 13)   13))


;; --- Exercício 2.8 ---------------------

;; Crie uma função recursiva soma-lista (abaixo) que, dada uma lista de números,
;; calcula a soma dos números contidos

(define (soma-lista-2 lst)
  (define (soma-lista-acc lst acc)
    (if (empty? lst)
        acc
        (soma-lista-acc (rest lst) (+ (first lst) acc))))
  (soma-lista-acc lst 0))

(define-test-suite testes-soma-lista
  (test-equal? "soma da lista vazia"                (soma-lista '())                  0)
  (test-equal? "soma de um número apenas"           (soma-lista '(13))                13)
  (test-equal? "soma de vários números"             (soma-lista (list 5 4 3 2 1))     15)
  (test-equal? "soma de números em ordem diferente" (soma-lista (list 1 2 3 4 5))     15)
  (test-equal? "soma de lista com zero"             (soma-lista (list 1 0 2 0 13 0))  16))

;; --- Exercício 2.9 ---------------------

;; Crie uma função recursiva mult-lista (abaixo) que, dada uma lista de números,
;; calcula o produto dos números contidos (a lista vazia deve ter produto igual a 1)

(define (mult-lista l)
  (define (mult-lista l current)
    (if (empty? l)
        current
        (mult-lista (rest l) (* current (first l)))))
  (mult-lista l 1))

(define-test-suite testes-mult-lista
  (test-equal? "produto da lista vazia"            (mult-lista '())                  1)
  (test-equal? "produto de lista com zero"         (mult-lista (list 1 0 2 0 13 0))  0)
  (test-equal? "produto de um número"              (mult-lista '(55))                55)
  (test-equal? "produto de vários números"         (mult-lista (list 1 2 3 4 5))     120)
  (test-equal? "produto de números em outra ordem" (mult-lista (list 2 5 1 4 3))     120))


;; --- Exercício 2.10 --------------------

;; Crie uma função recursiva max-lista (abaixo) que, dada uma lista de números naturais,
;; calcula o maior número entre os presentes na lista. Use (max-lista '()) = 0.
(define (max-lista l)
  (define (max-lista l currentMax)
    (cond
      [(empty? l) currentMax]
      [(<= (first l) currentMax) (max-lista (rest l) currentMax)]
      [(>= (first l) currentMax) (max-lista (rest l) (first l))]
      )
    )
  (max-lista l 0))

(define-test-suite testes-max-lista
  (test-equal? "maximo da lista vazia"       (max-lista '())                     0)
  (test-equal? "maximo de lista unitaria"    (max-lista '(22))                   22)
  (test-equal? "maximo de lista com numeros" (max-lista (list 8 55 13 24 45))    55)
  (test-equal? "maximo não muda com ordem"   (max-lista (list 45 13 8 55 24))    55))
  (test-equal? "maximo de lista com zeros"   (max-lista (list 1 0 13 0 356 0))   356)