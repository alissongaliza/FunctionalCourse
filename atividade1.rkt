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
;;Forçando que o @x e @y sejam números e que a funcao retorne um número

;;versao nativa para comparacao nos testes unitarios
(define/contract
  (potenciaNativa x y)
  (number? (and/c number?) . -> . number?)
  (expt x y))

(define/contract
  (potenciaCustom x y)
  (number? (and/c number?) . -> . number?)
  (cond
    [(= y 1) x]
    [(= y 0) 1]
    [(> y 1) (* x (potenciaCustom x (- y 1)))] ;;utilizando o expoente como contador da recursão
    [(< y 0) (/ 1 (potenciaCustom x (- 0 y)))]
    ;;explicando a gambiarra acima: se for menor que 0, eu divido por 1 e rodo a funcao
    ;;pelo módulo do expoente
    ;;exemplificando: 2⁻² => 1/2² => 1/4
  ))

;;escrevendo testes de unidade para a funcao de potenciaCustom
;;material de referencia: docs.racket-lang.org/rackunit/api.html#%28part._.Overview_of_.Rack.Unit
(test-begin 
 "Teste - questao 8"
 (check-eqv? (potenciaCustom 2 8) (potenciaNativa 2 8) "Valor retornado diferente do esperado")
 (check-eqv? (potenciaCustom 6 0) (potenciaNativa 6 0) "Valor retornado diferente do esperado")
 (check-eqv? (potenciaCustom 7 -2) (potenciaNativa 7 -2) "Valor retornado diferente do esperado")
 "Testes questão 8 - OK"
 )