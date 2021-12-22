;;; This file was generated by writeminikanren.pl
;;; Generated at 2007-10-25 15:24:42
(define nl (string #\newline))

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define-syntax lambdag@
  (syntax-rules (:)
    ((_ (n f c) e ...) (lambda (n f c) e ...))
    ((_ (n f c : S F G) e ...)
     (lambda (n f c)
       (let ((S (c->S c))
             (F (c->F c))
             (G (c->G c)))
         e ...)))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e ...) (lambda () e ...))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

(define lhs (lambda (pr) (car pr)))

(define rhs (lambda (pr) (cdr pr)))

(define var (lambda (dummy) (vector dummy)))

(define var? (lambda (x) (vector? x)))

(define c->S (lambda (c) (car c)))

(define c->F (lambda (c) (cadr c)))

(define c->G (lambda (c) (caddr c)))

(define empty-s '())

(define empty-c '(() () ()))

(define walk
  (lambda (u S)
    (cond
      ((and (var? u) (assq u S)) =>
       (lambda (pr) (walk (rhs pr) S)))
      (else u))))

(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define unify
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
        ((eq? u v) s)
        ((var? u) (ext-s-check u v s))
        ((var? v) (ext-s-check v u s))
        ((and (pair? u) (pair? v))
         (let ((s (unify 
                    (car u) (car v) s)))
           (and s (unify 
                    (cdr u) (cdr v) s))))
        ((equal? u v) s)
        (else #f)))))

(define ext-s-check
  (lambda (x v s)
    (cond
      ((occurs-check x v s) #f)
      (else (ext-s x v s)))))

(define occurs-check
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v) 
         (or 
           (occurs-check x (car v) s)
           (occurs-check x (cdr v) s)))
        (else #f)))))

(define walk*
  (lambda (w s)
    (let ((v (walk w s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
           (walk* (car v) s)
           (walk* (cdr v) s)))
        (else v)))))

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v)
         (ext-s v (reify-name (length s)) s))
        ((pair? v) (reify-s (cdr v)
                     (reify-s (car v) s)))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define reify
  (lambda (v)
    (lambda (c)
      (let ((S (c->S c)))
        (let ((v (walk* v S)))
          (walk* v (reify-s v empty-s)))))))

; Types of infinity stream
; zero
(define mzero (lambda () #f))

; one
(define unit (lambdag@ (n f c) c))

; one or more
(define choice (lambda (c f) (cons c f)))

; incomplete stream
(define-syntax inc 
  (syntax-rules () ((_ e) (lambdaf@ () e))))
 
(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((a^) e2) ((a f) e3))
     (let ((a-inf e))
       (cond
         ((not a-inf) e0)
         ((procedure? a-inf)  (let ((f^ a-inf)) e1))
         ((not (and (pair? a-inf)
                    (procedure? (cdr a-inf))))
          (let ((a^ a-inf)) e2))
         (else (let ((a (car a-inf)) (f (cdr a-inf))) 
                 e3)))))))

(define empty-f (lambdaf@ () (mzero)))

(define (fetch-predicate tracking-set)
  (if (null? tracking-set)
      #f
      (get-key (car tracking-set))))

(define (construct-var-list n)
  (if (= n 0)
      `()
      (cons (var (string->symbol (string-append "temp_" (number->string n)))) (construct-var-list (- n 1)))))

(define last-step
  (lambda (tracking-set x)
    (lambdag@ (dummy frame final-c : S F G)
      (let ((g (fetch-predicate tracking-set)))
        (if (and g #t)
            (inc 
             (mplus* 
               (bind* -1 '() ((apply (eval (get-key g)) (construct-var-list (get-value g))) -1 '() final-c) (last-step (cdr tracking-set) x))
             ))
            (let ((z ((reify x) final-c)))
                  (choice z empty-f))))
    )
  )
)

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (take n
       (lambdaf@ ()
         ((fresh (x) g0 g ... 
            (last-step tracking x))
          0 `() `(() () 0) 
         ))))))

(define take
  (lambda (n f)
    (if (and n (zero? n)) 
      '()
      (case-inf (f)
        (() '())
        ((f) (take n f))
        ((a) (cons a '()))
        ((a f)
         (cons a
           (take (and n (- n 1)) f)))))))

(define ==
  (lambda (u v)
    (lambdag@ (n f c : S F G)
      (if (even? n)
        (cond
          ((unify u v S) => (post-unify-== c S F G))
          (else (mzero)))
        (cond
          ((unify u v S) => (lambda (s) (mzero)))
          (else (unit n f c)))
      )  
    )))

(define post-unify-==
  (lambda (c S F G) 
    (lambda (S+) 
      (unit 0 '() (list S+ F S)))))
 
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (n f s)
       (inc
         (let ((x (var 'x)) ...)
           (bind* n f (g0 n f s) g ...)))))))
 
(define-syntax bind*
  (syntax-rules ()
    ((_ n f e) e)
    ((_ n f e g0 g ...) (bind* n f (bind n f e g0) g ...))))
 
(define bind
  (lambda (n fr a-inf g)
    (case-inf a-inf
      (() (mzero))
      ((f) (inc (bind n fr (f) g)))
      ((a) (g n fr a))
      ((a f) (mplus (g n fr a) (lambdaf@ () (bind n fr (f) g)))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (n f s) 
       (inc 
         (mplus* 
           (bind* n f (g0 n f s) g ...)
           (bind* n f (g1 n f s) g^ ...) ...))))))

(define-syntax conde-t
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (fresh ()
      (trans-fresh g0 g ...)
      (trans-fresh g1 g^ ...) ...))))

(define-syntax trans-conde
  (syntax-rules (conde)
    ((_ (conde (g0 g ...) (g1 g^ ...) ...)) (conde-t (g0 g ...) (g1 g^ ...) ...))
  ))

(define (find-bound-vars argv S)
  (if (null? argv)
      '()
      (let ((key (walk (car argv) S)))
        (if (not (var? key))
            (cons (car argv) (find-bound-vars (cdr argv) S))
            (find-bound-vars (cdr argv) S)))))

(define (remove-var var S)
  (if (null? S)
      '()
      (if (equal? var (car (car S)))
          (remove-var var (cdr S))
          (cons (car S) (remove-var var (cdr S))))))

(define-syntax fresh-t
  (syntax-rules ()
    ((_ (x ...) g0)
     g0)
    ((_ (x ...) g0 g ...)
     (fresh ()
      (conde [g0] [(fresh () 
                      (no g0)
                      (lambdag@ (n f c : S F G)
                        (define helper
                          (lambda (var values)
                            (lambdag@(nn ff cc : SS FF GG)
                              (if (null? values)
                                  (list G FF GG)
                                  (inc 
                                   (mplus* 
                                     (bind* n f ((fresh-t (x ...) g ...) n f (list (ext-s var (car values) G) FF G)) (helper var (cdr values)))
                                   ))
                              )
                              
                            )
                          )
                        )
                        (let ((argv (list x ...)))
                          (let ((bounded-vars (find-bound-vars argv S)))
                            (if (null? bounded-vars)
                                ((fresh-t (x ...) g ...) n f c)
                                (begin
                                  ; [ToDo] Add multiple variables support.
                                  (cout (car bounded-vars) nl)
                                  (cout "remove var:" (car bounded-vars) "from S:" S nl)
                                  (cout "removed S:" (remove-var (car bounded-vars) S) nl)
                                  (cout "Old S:" G nl)
                                  (let ((domain-of-var (take #f (lambdaf@ () ((fresh () (no g0) (last-step '() (car bounded-vars))) n f (list G F G))))))
                                    (cout domain-of-var nl)
                                    ((helper (car bounded-vars) domain-of-var) n f c)
                                    ;((fresh-t (x ...) g ...) n f (list (ext-s (car bounded-vars) (car domain-of-var) G) F G))
                                  )
                                )
                            )
                          )
                        )
                      )
                    )]) 
                      ;(fresh-t (x ...) g ...))])
     ))
  ))

(define-syntax trans-fresh
  (syntax-rules (fresh)
    ((_ (fresh (x ...) g0 g ...)) (fresh (x ...) 
                                      (fresh-t (x ...) g0 g ...)))
    ((_ g0 g ...) (fresh-t () g0 g ...))
  ))

(define-syntax check-negation
  (syntax-rules ()
    ((_ (g0 ...)) (let ((name (car `(g0 ...))))
        (if (equal? name 'no)
            #t
            #f)))
    ((_ (g0 ...) (g1 ...) ...) 
      (let ((name (car `(g0 ...))))
        (if (equal? name 'no)
            #t
            (check-negation (g1 ...) ...))))
  )
)

(define-syntax filter-negation
  (syntax-rules ()
    ((_ g0 g1 ...) (lambdag@ (n f c)
                      (if (check-negation g0 g1 ...)
                        ((trans-fresh g0 g1 ...) n f c)
                        (fail n f c)))))
)

(define-syntax global-checking
  (syntax-rules ()
    ((_ (conde (g0 g ...) (g1 g^ ...) ...) (name args ...) )
      (fresh () (filter-negation g0 g ... (name args ...) ) (filter-negation g1 g^ ... (name args ...)) ...)
    ))
)
 
(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus e0 
                    (lambdaf@ () (mplus* e ...))))))
 
(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
      (() (f))
      ((f^) (inc (mplus (f) f^)))
      ((a) (choice a f))
      ((a f^) (choice a (lambdaf@ () (mplus (f) f^)))))))

(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (n f s)
       (inc
         (ifa n f ((g0 n f s) g ...)
              ((g1 n f s) g^ ...) ...))))))
 
(define-syntax ifa
  (syntax-rules ()
    ((_ n fr) (mzero))
    ((_ n fr (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifa n fr b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* n fr a-inf g ...))
         ((a f) (bind* n fr a-inf g ...)))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (n f s)
       (inc
         (ifu n f ((g0 n f s) g ...)
              ((g1 n f s) g^ ...) ...))))))
 
(define-syntax ifu
  (syntax-rules ()
    ((_ n fr) (mzero))
    ((_ n fr (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifu n fr b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* n fr a-inf g ...))
         ((a f) (bind* n fr (unit n fr a) g ...)))))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambdag@ (n fr c : S F G)
       (let ((x (walk* x S)) ...)
         ((fresh () g g* ...) n fr c))))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define onceo
  (lambda (g)
    (condu
      (g succeed)
      ((== #f #f) fail))))

;;; Representing sets as unordered lists.
; O(n) complexity.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (get-key (car set))) (car set))
        (else (element-of-set? x (cdr set)))))

; O(1) complexity.
(define (adjoin-set x set) (cons x set))

(define (make-record key value)
  (cons key value))

(define (get-key record)
  (car record))

(define (get-value record)
  (cdr record))

(define (set-value record value)
  (set-cdr! record value))

; Record the procedure we produced the result.
; [ToDo] Propositional only, add predicate support.
(define update-F
  (lambda (name argv)
    (lambdag@(n f c : S F G)
      (let ((key (map (lambda (arg)
                              (walk arg S)) argv) ))
        (cout "update-F: " name key nl)
        (list S (adjoin-set (make-record (list name key) n) F) G)
      )
    )))

; Unavoidable OLON in the program.
(define tracking `())

(define-syntax def-asp-rule
  (syntax-rules ()
    ((_ (name args ...) exp ...)
      (begin
        ; Add rule to tracking set.
        (set! tracking (adjoin-set (make-record 
            (make-record `name (length (list `args ...))) 0) tracking))
        (define name (lambda (args ...)
          ; Coinduction and tabling.
          (let ((argv (list args ...))
                (alt_name (string-append "co_" 
                          (symbol->string `name))))
            (lambdag@ (n f c : S F G)
              ; Inspect calling stack.
              ;(display S)
              (let ((key (map (lambda (arg)
                              (walk arg S)) argv) ))
                (let ((result (element-of-set? (list `name key) F)))
                  (if (and result #t)
                    (cond ((< n 0) (unit n f c))
                          ((and (even? (get-value result)) (even? n)) (unit n f c))
                          ((and (even? (get-value result)) (odd? n)) (mzero))
                          ((and (odd? (get-value result)) (even? n)) (mzero))
                          ((and (odd? (get-value result)) (odd? n)) (unit n f c)))
                    (let ((record (element-of-set? (list `name key) f)))
                      (if (and record #t) 
                        (let ((diff (- n (get-value record))))
                          (cond 
                            ; Positive loop (minimal model semantics)
                            ((= 0 diff)
                              (if (even? n)
                                  (mzero)
                                  (unit n f c)))
                            ; Negative loop (Odd co-inductive failure)
                            ((odd? diff) (mzero))
                            ; Negative loop (Even co-inductive success)
                            (else (choice c mzero)))
                        )
                        ; Expand calling stack.
                        ((cond ((< n 0) (begin (cout 'nmr nl) (global-checking exp ... (name args ...) )))
                              ((even? n) (begin (cout `name nl) (fresh () exp ... (update-F `name argv))))
                              (else (begin (cout alt_name nl) (fresh () (trans-conde exp ...) (update-F `name argv)))
                              ))
                        ;(if (even? n)
                        ;  (begin (display `name) (fresh () exp ... (update-F `name argv)))
                        ;  ;(begin (display alt_name) ((eval (string->symbol alt_name)) args ...))
                        ;  ; Define a transformed rule. [def-asp-complement-rule]
                        ;  (begin (display alt_name) (fresh () (trans-conde exp ...) (update-F `name argv)))
                        ; ) 
                         (if (< n 0)
                          1
                          n) (adjoin-set 
                                        (make-record
                                          (list `name key)
                                          n) f) c)
                      )
                    )
                  )
                )
              )
            ))
          ))
       ))))

(define-syntax no
  (syntax-rules ()
    ((no (name args ...))
      (lambdag@ (n f c)
        (let ((newN (+ 1 n)))
          (display newN)
          ((name args ...) newN f c)
        )
      )
    )))
