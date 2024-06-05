
(define-syntax test-remove-negation
  (syntax-rules ()
    ((_ (name args ...) exp ...)
      (define (name args ...)
        (remove-negation exp ...)
      )
    )))


(define (q x)
  (conde 
    [(== x 1)]
    [(== x 2)]))

(def-asp-rule (f x)
    (conde [(no (p x))]
           [(q x)]
           [(fresh (y) (no (r x y)))]))

(test-remove-negation (edge x y)
  (conde [(== x 'a) (== y 'b)]
         [(== x 'b) (== y 'c)]
         [(== x 'c) (== y 'd)]))
