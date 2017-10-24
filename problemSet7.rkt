;Problem Set 7
;Due 30 October 2017
;Terry Pireaux

;test tree
(define example (list #\+
                        (list #\*
                                   (list 4 '() '())
                                   (list 5 '() '()))
                        (list #\+
                               (list #\/
                                     (list 6 '() '())
                                     (list 3 '() '()))
                               (list 7 '() '()))

                        ))
(null? (cadr (list 1 '() '())))

;Tree clarification-helpers
; T = (list node '(left) '(right))
; T = (list (car t) '(cadr T) '(caddr T))
(define (node T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))

"problem 1"
;find the expression denoted by the arithmetic parse tree
(define (nvalue T)
  (cond ((number? (node T))
         (node T))
        ((eq? (node T) #\+)
         (+ (nvalue (left T))
            (nvalue (right T))))
        ((eq? (node T) #\*)
         (* (nvalue (left T))
            (nvalue (right T))))
        (else (if (eq? (node T) #\-)
                  (* -1  (nvalue (left T)))
                  (/ 1 (nvalue (left T)))))
        ))

"(nvalue example)"
(nvalue example)

"problem 2a"
;helper function that turns an input into a string
(define (prepare x)
   (cond ((number? x) (number->string x))
          ((char? x) (string x))))
;start p2
;read the tree and create a string representative
;of prefix notation a.k.a polish notation
;That is, call vist the root node first
;then visit the left node
;then visit the right node
(define (prefix T)
  (cond ((null? T) 
         '())
        ((null? (cadr T))
         (prepare (node T)))
         (else (string-append (prepare (node T))
                              (prefix (left T))
                              (prefix (right T))))))

"test"

"(prefix example)"
(prefix example)
 
"problem 2b"
;read the tree and create a string representative
;of prefix notation a.k.a reverse polish notation
(define (postfix T)
  (cond ((null? T) 
         '())
        ((null? (cadr T))
         (prepare (node T)))
         (else (string-append (postfix (left T))
                              (postfix (right T))
                              (prepare (node T))))))
"test"
"(postfix example)"
(postfix example)
               
                
                                               