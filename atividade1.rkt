#lang racket
(require rackunit)

;1
(define a 42)

;2
(define b 14)

;;3.1
(define exp1 (+ a b))
;;3.2
(define exp2 (- a b))
;;3.3
(define exp3 (+ a (* 3 b) 7))
;;3.4
(define mediaAritmetica (/ (+ a b) 2))
;;3.5
(define mediaGeometrica (sqrt (* a b)))
;;3.6
(define mediaHarmonica (/ 2 (+ (/ 1 a) (/ 1 b))))

;;4
(define soma-medias (+ (/ (+ a b) 2) (/ 2 (+ (/ 1 a) (/ 1 b)))))

;;5
(if (equal? soma-medias 49) "teste 1 ok" "teste 1 falhou")

;;6
(define (quadrado x) (* x x))

;;7
(define (delta a b c)
  (- (quadrado b) (* 4 a c)))
(define (raiz-positiva a b c)
  (/ (+ (- b) (sqrt (delta a b c))) (* 2 a)))

;;8
;;Utilizando contratos para limitar inputs
;;links utilizados: beautifulracket.com/explainer/contracts.html
;; e docs.racket-lang.org/reference/function-contracts.html
;;Forçando que o @x seja um numero, que o @y seja um número positivo
;; e que a funcao retorne um numero
(define/contract
  (potenciaNativa x y)
  (number? (and/c number? positive?) . -> . number?)
  (expt x y))

(define/contract
  (potenciaCustom x y)
  (number? (and/c number? positive?) . -> . number?)
  (if (= y 1) x 
      (* x 
         (potenciaCustom x 
                         (- y 1)))))

;;escrevendo testes de unidade para a funcao de potenciaCustom
;;material de referencia: docs.racket-lang.org/rackunit/api.html#%28part._.Overview_of_.Rack.Unit
(test-begin 
 "Teste - questao 8"
 (check-eqv? (potenciaCustom 2 8) (potenciaNativa 2 8) "Valor retornado diferente do esperado"))