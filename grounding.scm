(define (anyvalue? v)
    (if (not (symbol? v)) #f
        (string-prefix? (string->list "_.") (string->list (symbol->string v)))))

(define (string-prefix? p s)
    (if (null? p) #t
        (if (equal? (car p) (car s))
            (string-prefix? (cdr p) (cdr s))
            #f)))

(define-syntax remove-negation
  (syntax-rules (conde)
    ((_ (conde (g0 g ...) (g1 g^ ...) ...))
        (conde [(remove-negation g0 g ...)]
               [(remove-negation g1 g^ ...)]
             ...))
    ((_ (fresh (x ...) g0 g ...))
        (fresh (x ...) (remove-negation g0 g ...)))
    ((_ (g0 ...)) 
      (let ((name (car `(g0 ...))))
        (if (equal? name 'no)
            succeed
            (g0 ...))))
    ((_ (g0 ...) (g1 ...) ...) 
      (let ((name (car `(g0 ...))))
        (fresh () 
          (if (equal? name 'no)
            succeed
            (g0 ...))
        (remove-negation (g1 ...) ...))))
    ))
