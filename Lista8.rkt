#lang racket
 (require racket/dict)
;Zadanie 1.
;Zdefiniuj procedurę mreverse!, która odwraca listę mutowalną „w miejscu”, czyli nie
;tworzy nowych bloczków mcons-em, a odpowiednio przepina wskaźniki.

(define (mreverse! xs)
  (define (it q prev next)
    (cond [(null? next)
           (set-mcdr! q prev) q]
          [else(set-mcdr! q prev)
               (it next q (mcdr next))]))
  (it xs null (mcdr xs)))
(define m (mcons 1 (mcons 2 (mcons 3 null))))
(mreverse! m)


;Zadanie 2. (2 pkt)
;Wzorując się na implementacji kolejek z wykładu zaimplementuj kolejki dwukierunkowe, czyli takie w których można wstawiać i usuwać element zarówno z jednej jak i z
;drugiej strony kolejki. Do implementacji kolejek dwukierunkowych użyj list dwukierunkowych, czyli takich w których każdy węzeł ma wskaźnik na następny i poprzedni
;węzeł listy.
;Twoja implementacja powinna znajdować się w osobnym module, a eksportowane
;procedury powinny mieć odpowiednie kontrakty.

;Zadanie 3. (2 pkt)
;Podziel moduł parser.rkt z wykładu na dwa moduły:
;• parsing.rkt — zawierający ogólne definicje dotyczące parsowania,
;• expparser.rkt — zawierający konkretne zastosowanie modułu parsing.rkt do
;parsowania wyrażeń arytmetycznych.
;Zwróć uwagę na to, które definicje powinny zostać wyeksportowane, a które powinny
;zostać prywatne dla modułu. W module parsing.rkt zadbaj o odpowiednie kontrakty
;dla eksportowanych procedur.

;Zadanie 4.
;Zmodyfikuj parser wyrażeń arytmetycznych z wykładu tak, by nie konstruował abstrakcyjnego drzewa rozbioru (drzewa typu Exp), ale od razu obliczał podane wyrażenie
;do liczby. Zauważ, że wystarczy zmodyfikować tylko akcje semantyczne podane w poszczególnych regułach.
;MP22 @ II UWr Lista 8

;Zadanie 5. (2 pkt)
;Parser pokazany na wykładzie nie jest jeszcze idealny, ale poprawimy go na następnym
;wykładzie. Jednak jedną z jego wad możemy poprawić teraz. W opisie gramatyki obecnie
;znajduje się następująca definicja wyrażeń.
;(" expression "
;((" simple-expr " " operator " " simple-expr ")
;,( lambda ( e1 op e2 ) ( exp-op op e1 e2 ) ) )
;((" simple-expr ") ,( lambda ( e ) e ) ) )
;Zauważ, że jeśli wyrażenie jest wyrażeniem prostym, to parser zacznie od pierwszej
;reguły, gdzie sparsuje podane wyrażenie proste, ale zawiedzie gdy nie uda mu się
;znaleźć operatora. Wtedy przejdzie do drugiej reguły, której wykonanie się powiedzie.
;W efekcie, parsowane wyrażenie będzie dwukrotnie odwiedzane przez parser. Jeśli
;problem będzie się powtarzał z każdym wywołaniem rekurencyjnym, to możemy się
;zderzyć z wykładniczym spowolnieniem programu.
;> ( run-exp-parser '((((((((((((((((((((((((42) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
;Można temu problemowi zaradzić modyfikując gramatykę wyrażeń: wyrażenie to wyrażenie proste po którym opcjonalnie występuje operator i kolejne wyrażenie proste,
;co w naszym języku opisu gramatyk można zapisać następująco:
;(" expression "
;((" simple-expr " " expr-rest ") ,(...) ) )
;(" expr-rest "
;((" operator " " simple-expr ") ,(...) )
;(() ,(...) ) )
;Uzupełnij akcje semantyczne tak, by zmodyfikowany parser dalej poprawnie konstruował wyrażenia. Pamiętaj, że wartością funkcji może też być funkcja.

;Zadanie 6. (2 pkt)
;Napisz funkcję tłumaczącą napis złożony z liter, cyfr, białych znaków i wybranych znaków interpunkcyjnych na kod Morse’a. Kropkę koduj jako znak kropki (#\.), kreskę jako
;znak podkreślenia (#\_), odstęp między znakami jako pojedynczą spację, zaś dowolny
;ciąg białych znaków jako dwie spacje. Oto przykładowe wywołanie takiej procedury:
;> ( morse-code " Metody Programowania ")
;"__ . _ ___ _.. _.__ .__. ._. ___ __. ._. ._ __ ___ .__ ._ _. .. ._"
;Przydatne mogą okazać się procedury string->list, list->string oraz char-whitespace?.
;Postaraj się zredukować złożoność kodu poprzez zakodowanie części algorytmu za
;pomocą danych.
;2
;MP22 @ II UWr Lista 8
(define (morse-code napis)
  (coder (string->list napis))
  )

(define (coder lista)
  (cond[(empty? lista) ""]
       [(equal? (car lista) #\space)
        (coder (cdr lista))]
       [(eq? (assoc (string (car lista) ) slownik2 ) #f)
        ""]
       [#t
        (string-append " "(cdr (assoc (string (car lista) ) slownik2 ) ) (coder (cdr lista) ) )]
       )
  )



;Zadanie 7. (2 pkt)
;Napisz funkcję tłumaczącą kod Morse’a na zwykły napis, np.
;> ( morse-decode "__ .__. .. ___ _____ .. ___ .. ___ ")
;"MP 2022 "
;Również postaraj się zakodować część algorytmu za pomocą odpowiednich danych.
;(define (fold-tree func x xtree) (cond [(leaf? xtree) x]
;                                        [(node? xtree)
;                                         (func (fold-tree func x (node-l xtree))(node-elem xtree) (fold-tree func x (node-r xtree)))]))
;
(define slownik
  (list
   (cons "._"  "A")
   (cons "_..."  "B")
   (cons "_._."  "C")
   (cons "_.."  "D")
   (cons "."  "E")
   (cons ".._."  "F")
   (cons "__."  "G")
   (cons "...."  "H")
   (cons ".."  "I")
   (cons ".___"  "J")
   (cons "_._"  "K")
   (cons "._.."  "L")
   (cons "__"  "M")
   (cons "_."  "N")
   (cons "___"  "O")
   (cons ".__."  "P")
   (cons "__._"  "Q")
   (cons "._."  "R")
   (cons "..."  "S")
   (cons "_"  "T")
   (cons ".._"  "U")
   (cons "..._"  "V")
   (cons ".__"  "W")
   (cons "_.._"  "X")
   (cons "_.__"  "Y")
   (cons "__.."  "Z")
   (cons "_____"  "0")
   (cons ".____"  "1")
   (cons "..___"  "2")
   (cons "...__"  "3")
   (cons "...._"  "4")
   (cons "....."  "5")
   (cons "_...."  "6")
   (cons "__..."  "7")
   (cons "___.."  "8")
   (cons "____."  "9")
   
   )
  )
(define slownik2
  (list
   (cons "A" "._")
   (cons "B" "_...")
   (cons "C" "_._."  )
   (cons "D" "_.."  )
   (cons "E" ".")
   (cons "F" ".._."  )
   (cons "G" "__."  )
   (cons "H" "...."  )
   (cons "I" ".."  )
   (cons "J" ".___"  )
   (cons "K" "_._"  )
   (cons "L" "._.."  )
   (cons "M" "__"  )
   (cons "N" "_."  )
   (cons "O" "___"  )
   (cons "P" ".__."  )
   (cons "Q" "__._"  )
   (cons "R" "._."  )
   (cons "S" "..."  )
   (cons "T" "_"  )
   (cons "U" ".._"  )
   (cons "V" "..._"  )
   (cons "W" ".__"  )
   (cons "X" "_.._"  )
   (cons "Y" "_.__"  )
   (cons "Z" "__.."  )
   (cons "0" "_____"  )
   (cons "1" ".____"  )
   (cons "2" "..___"  )
   (cons "3" "...__"  )
   (cons "4" "...._"  )
   (cons "5" "....."  )
   (cons "6" "_...."  )
   (cons "7" "__..."  )
   (cons "8" "___.."  )
   (cons "9" "____."  )
   (cons "." "._._._")
   (cons "," "__..__")
   
   )
  )
   ;(cons (list #\space ) #\B)
   ;(cons (list #\space ) #\C)
   ;(cons (list #\space ) #\D)
   ;(cons (list #\space ) #\E)
   ;(cons (list #\space ) #\F)
   ;;(cons (list #\space ) #\G)
   ;(cons (list #\space ) #\H)
   ;(cons (list #\space ) #\I)
   ;(cons (list #\space ) #\J)
   ;(cons (list #\space ) #\K)
   ;(cons (list #\space ) #\L)
   ;(cons (list #\space ) #\M)
   ;(cons (list #\space ) #\N)
   ;(cons (list #\space ) #\O)
   ;(cons (list #\space ) #\P)
   ;(cons (list #\space ) #\R)
   ;(cons (list #\space ) #\S)
   ;(cons (list #\space ) #\T)
   ;(cons (list #\space ) #\U)
   ;(cons (list #\space ) #\W)
   ;(cons (list #\space ) #\V)
  ; (cons (list #\space ) #\X)
   ;(cons (list #\space ) #\Y)
  ; (cons (list #\space ) #\Z)
   ;(cons (list #\space ) #\1)
   ;(cons (list #\space ) #\2)
  ; (cons (list #\space ) #\3)
   ;(cons (list #\space ) #\4)
   ;(cons (list #\space ) #\5)
   ;(cons (list #\space ) #\6)
   ;(cons (list #\space ) #\7)
  ; (cons (list #\space ) #\8)
   ;(cons (list #\space ) #\9)
   ;(cons (list #\space ) #\0)
   ;))
   



;(define-struct leaf (litera))
;(define-struct node (kropka litera kreska))
;(define-struct alfabet(node (node (node (node (node (leaf "5") "H" (leaf "4") )
;                                              "S" (node (leaf "") "V" (leaf "3") ) )
;                                        "I"  )
;                                  "E" )
;                           "." () )
;  #:transparent)
;
(define (szukanie-spacji kod poczatek indeks pamiec)
  (cond[(equal? (car kod) #\space) (substring pamiec poczatek (+ 1 indeks) )]
       [(empty? kod) null]
       [#t (szukanie-spacji (cdr kod) poczatek (+ 1 indeks) pamiec)]
    )
  )

(define (split x result elem)
  [cond
    [[empty? x] (append result (list elem))]
    [[equal? (first x) #\space] (split (rest x) (append result(list elem )) '())]
    [else (split (rest x) result (append elem (list (first x))))]
    ])


  
(define (morse-fold kod)
  (cond[(empty? kod)
        ""]
       [(eq? #f (assoc (list->string (car kod))  slownik ))
        (string-append " " (morse-fold (cdr kod) ))]
       [#t
        (string-append (cdr (assoc (list->string (car kod))  slownik ))
                       (morse-fold (cdr kod) ) )]
     )
  )

(define (morse-decode kod)
  (morse-fold (split (string->list kod) '() '() ))
  )