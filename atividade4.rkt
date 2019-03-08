#lang racket
(require rackunit rackunit/text-ui)

;;Helpers

(define (addToLast lista el)
  (append lista (list el)))

(define (pertence? x lst)
  (cond
    [(empty? lst) #f]
    [(equal? (first lst) x) #t]
    [else (pertence? x (rest lst))]
    )
  )

(define (conjunto=? c1 c2)
  (cond
    [(and (empty? c1) (empty? c2)) #t]
    [(and (empty? c1) (not (empty? c2))) #f]
    [(pertence? (first c1) c2) (conjunto=? (rest c1) (remove-primeiro (first c1) c2))]
    [else #f]
  )
)

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

(define (soma-lista lst)
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

;; --- Exercício 2.12 --------------------

;; Muitas vezes precisamos transformar os elementos de uma lista da mesma
;; maneira. Escreva a função quadrado-lista (abaixo) que, dada uma lista de
;; números, obtém uma lista contendo o quadrado de cada número da lista
;; original (nas mesmas posições)
(define (quadrado-lista l)
  (define (quadrado-lista l current)
    (if (empty? l)
        current
        (quadrado-lista (rest l) (append current (list(* (first l)(first l)))) )
        )
    )
  (quadrado-lista l '())
  )

(define-test-suite testes-quadrado-lista
  (test-equal? "quadrado da lista vazia"  (quadrado-lista '())        '())
  (test-equal? "quadrado de um número"    (quadrado-lista '(5))       '(25))
  (test-equal? "quadrado de números"
               (quadrado-lista (list 2 5 12 25))
               (list 4 25 144 625)))


;; --- Exercício 2.13 --------------------

;; Agora vamos selecionar itens em uma lista. Crie uma função filtra-par (abaixo)
;; que, dado uma lista de números naturais, retorna uma outra lista contendo apenas
;; os números pares da lista original. Use a função par definida no exercício 3

(define (par n)
  (if (= n 0) #t (if (negative? n) #f (par (- n 2)))))


(define (filtra-par l)
  (define (filtra-par l current)
    (if (empty? l) current
        (if (par (first l))
            (filtra-par (rest l) (append current (list (first l))))
            (filtra-par (rest l) current)
            )))
  (filtra-par l '())
  )

(define-test-suite testes-filtra-par
  (test-equal? "filtragem da lista vazia"     (filtra-par '())                  '())
  (test-equal? "filtragem de lista sem pares" (filtra-par (list 1 3 5 7 9))     '())
  (test-equal? "filtragem de lista com pares" (filtra-par (list 1 2 3 4 5))     (list 2 4))
  (test-equal? "filtragem com todos os itens pares"
               (filtra-par (list 2 4 22 144))
               (list 2 4 22 144)))

;; --- Questão 3.1 ----------------------------

;; Escreva uma função remove-primeiro tal que
;; (remove-primeiro x lst) remove a primeira ocorrência do elemento x
;; na lista lst (se houver), retornando uma nova lista com o resultado.
;; Veja os testes para exemplos.
(define (remove-primeiro x lst)
  (define (remove-primeiro x lst current)
    (cond
      [(empty? lst) current]
      [(equal? (first lst) x) (append current (rest lst))]
      [else (remove-primeiro x (rest lst) (append current (list (first lst) )))]
      )
    )
  (remove-primeiro x lst '())
  )

(define-test-suite test-remove-primeiro
  (test-equal? "lista vazia"
               (remove-primeiro 5 '())              '())
  
  (test-equal? "uma ocorrência"
               (remove-primeiro 5 '(1 3 5 7))       '(1 3 7))
  
  (test-equal? "múltiplas ocorrências"
               (remove-primeiro 5 '(1 3 5 7 5 9))   '(1 3 7 5 9))
  
  (test-equal? "nenhuma ocorrência"
               (remove-primeiro 3 '(11 7 23 55 42)) '(11 7 23 55 42)))

;; --- Questão 3.2 ----------------------------

;; Escreva uma função remove-todos tal que
;; (remove-todos x lst) remove todas as ocorrencias do elemento x
;; na lista lst (se houver), retornando uma nova lista com o resultado.
(define (remove-todos x lst)
  (define (remove-todos x lst current)
    (cond
      [(empty? lst) current]
      [(equal? (first lst) x) (remove-todos x (rest lst) current)]
      [else (remove-todos x (rest lst) (append current (list (first lst) )))]
      )
    )
  (remove-todos x lst '())
  )

(define-test-suite test-remove-todos
  (test-equal? "lista vazia"           (remove-todos 5 '())              '())
  (test-equal? "uma ocorrência"        (remove-todos 5 '(1 3 5 7))       '(1 3 7))
  (test-equal? "múltiplas ocorrências" (remove-todos 5 '(1 3 5 7 5 9))   '(1 3 7 9))
  (test-equal? "nenhuma ocorrência"    (remove-todos 3 '(11 7 23 55 42)) '(11 7 23 55 42)))


;; --- Questão 3.5 ----------------------------

;; Infelizmente nem sempre podemos usar a mesma receita para recursividade. Um caso
;; comum são funções que devem combinar duas listas de alguma forma (como é o caso
;; das operações de união, intersecção e diferença de conjuntos).

;; Para praticar a ideia primeiro, escreva uma função combine tal que
;; (combine l1 l2) retorna uma lista de pares (listas de dois elementos) onde o primeiro
;; elemento de cada par vem de l1 e o segundo de l2. O número de pares deve ser igual ao
;; tamanho da menor lista. Veja os testes para exemplos.
(define (combine l1 l2)
  (define (combine l1 l2 current)
    (cond
      [(empty? l1) current]
      [(empty? l2) current]
      [else (combine (rest l1) (rest l2) (addToLast current (list (first l1) (first l2))))]
      )
    )
  (combine l1 l2 '())
  )

(define-test-suite test-combine
  (test-equal? "listas de mesmo tamanho"
               (combine '(1 2 3) '(10 20 30))  '((1 10) (2 20) (3 30)))
  
  (test-equal? "listas de tamanho diferente"
               (combine '(1)     '(55 33 11))  '((1 55)))
  
  (test-equal? "primeira lista vazia"
               (combine '()      '(1 2 3))     '())
  
  (test-equal? "segunda lista vazia"
               (combine '(1 2 3) '())          '())
  
  (test-equal? "segunda lista menor"
               (combine '(4 5 6) '(22 33))     '((4 22) (5 33))))


;; --- Questão 3.7 ----------------------------

;; Outra função de apoio que pode ser útil é uma que, dada uma
;; lista qualquer (podendo conter itens duplicados) retorna uma lista válida como
;; conjunto, sem itens duplicados. Podemos chamar essa função remove-duplicatas.

;; Escreva a função remove-duplicatas tal que
;; (remove-duplicatas lst) retorna uma lista com os mesmos elementos de lst mas
;; sem que nenhum item ocorra mais de uma vez.
(define (remove-duplicatas lst)
  (define (remove-duplicatas lst current)
    (cond
      [(empty? lst) current]
      [else (remove-duplicatas
             (remove-todos (first lst) (rest lst))
             (addToLast current (first lst))
             ) ]
      )
    )
  (remove-duplicatas lst '())
  )

;; Um outro nome para a mesma função poderia ser lista->conjunto, enfatizando a
;; sua aplicação na criação de conjuntos a partir de listas. Nesse caso podemos
;; definir um sinônimo para a mesma função acima
(define lista->conjunto remove-duplicatas)

;; Note que usamos conjunto=? nos testes, caso contrário funções que retornassem
;; elementos em ordens diferentes não passariam
(define-test-suite test-remove-duplicatas
  (test-true "sem duplicatas"
             (conjunto=? (remove-duplicatas '(1 2 3 4 5)) '(1 2 3 4 5)))
  
  (test-true "lista vazia"
             (conjunto=? (remove-duplicatas '()) '()))
  
  (test-true "várias duplicatas"
             (conjunto=? (remove-duplicatas '(1 2 3 2 3 5)) '(1 2 3 5)))
  
  (test-true "apenas um elemento"
             (conjunto=? (lista->conjunto   '(5 5 5 5 5 5)) '(5)))
  
  (test-true "mais repetições"
             (conjunto=? (lista->conjunto '(1 2 3 1 2 3 1 2 3 1 2 3)) '(1 2 3))))


;; --- Questão 3.8 -----------------------

;; Escreva uma função interseccao tal que
;; (interseccao c1 c2) retorna um conjunto contendo os elementos que ocorrem
;; em ambos c1 e c2
(define (interseccao c1 c2)
  (define (interseccao c1 c2 current)
    (cond
      [(or (empty? c1) (empty? c2)) current]
      [(if (pertence? (first c1) c2)
           (interseccao (rest c1) c2 (addToLast current (first c1)))
           (interseccao (rest c1) c2 current))]
      )
    )
  (interseccao c1 c2 '())
  )

(define-test-suite test-interseccao
  (test-equal? "Conjuntos vazios"        (interseccao '()      '())      '())
  (test-equal? "Intersecção com vazio 1" (interseccao '(1 2 3) '())      '())
  (test-equal? "Intersecção com vazio 2" (interseccao '()      '(11 22)) '())
  (test-equal? "Sem elementos comuns"    (interseccao '(1 2 3) '(11 22)) '())

  (test-true "Um elemento em comum"
             (conjunto=? (interseccao '(1 2 3) '(11 1 121))  '(1)))

  (test-true "Vários elementos em comum"
             (conjunto=? (interseccao '(1 3 5 7 9 11)
                                      '(11 3 1 13 17))
                         '(1 3 11)))

  (test-true "Mesmo conjunto"
             (conjunto=? (interseccao '(1 2 3 4 5) '(5 4 3 2 1))
                         '(1 2 3 4 5))))

;; --- Questão 3.9 -----------------------

;; Escreva uma função diferenca tal que
;; (diferenca c1 c2) retorna um conjunto que tem todos os elementos de c1 que
;; não pertencem a c2. Por exemplo, (diferenca '(1 3 5 7) '(3 7)) deve retornar
;; '(1 5) (não necessariamente nesta ordem).
(define (diferenca c1 c2)
  (define (diferenca c1 c2 current)
    (cond
      [(conjunto=? c1 c2) current]
      [(empty? c2) c1]
      [(empty? c1) current]
      [(if (not (pertence? (first c1) c2))
                (diferenca (rest c1) c2 (addToLast current (first c1)))
                (diferenca (rest c1) c2 current))]
      )
    )
  (diferenca c1 c2 '())
  )

;; Para esta função, escreva também um conjunto de testes, e adicione a suite de 
;; testes criados à execução de todos os testes, abaixo. Você pode escrever os
;; testes antes ou depois de implementar a função.
(define-test-suite test-diferenca
  (test-equal? "Diferenca com vazio"     (diferenca '(1 2 3) '())      '(1 2 3))
  (test-equal? "Sem elementos comuns"    (diferenca '(1 2 3) '(11 22)) '(1 2 3))
  (test-true "Um elemento em comum"      (conjunto=? (diferenca '(1 2 3) '(11 1 121))  '(2 3)))
  (test-true "Vários elementos em comum" (conjunto=? (diferenca '(1 3 5 7 9 11) '(11 3 1 13 17)) '(5 7 9)))
  (test-true "Mesmo conjunto"            (conjunto=? (diferenca '(1 2) '(2 1)) '())))

;; --- Executa todos os testes ---------
(run-tests
 (test-suite "todos os testes"
             testes-mult
             testes-soma-lista
             testes-mult-lista
             testes-max-lista
             testes-quadrado-lista
             testes-filtra-par
             test-remove-primeiro
             test-remove-todos
             test-combine
             test-remove-duplicatas
             test-interseccao
             test-diferenca
             ))


