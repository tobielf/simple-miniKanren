;;; This file was generated by writeminikanren.pl
;;; Generated at 2007-10-25 15:24:42

(define-syntax lambdag@
  (syntax-rules (:)
    ((_ (c) e ...) (lambda (c) e ...))
    ((_ (c : S N F) e ...)
     (lambda (c)
       (let ((S (c->S c))
             (N (c->N c))
             (F (c->F c)))
         e ...)))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

(define lhs (lambda (pr) (car pr)))

(define rhs (lambda (pr) (cdr pr)))

(define var (lambda (dummy) (vector dummy)))

(define var? (lambda (x) (vector? x)))

(define c->S (lambda (c) (car c)))

(define c->N (lambda (c) (cadr c)))

(define c->F (lambda (c) (caddr c)))

(define empty-s '())

(define empty-c '(() 0 () ))

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
(define unit (lambdag@ (c) c))

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

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (take n
       (lambdaf@ ()
         ((fresh (x) g0 g ... 
            (lambdag@ (final-c)
              (let ((z ((reify x) final-c)))
                (choice z empty-f))))
          empty-c))))))
 
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
    (lambdag@ (c : S N F)
      (cond
        ((unify u v S) => (post-unify-== c S N F))
        (else (mzero)))
      )))

(define not_==
  (lambda (u v)
    (lambdag@ (c : S N F)
      (cond
        ((unify u v S) => (lambda (s) (mzero)))
        (else (unit c)))
      )))

(define post-unify-==
  (lambda (c S N F) 
    (lambda (S+) 
      (unit (list S+ N F)))))
 
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (s)
       (inc
         (let ((x (var 'x)) ...)
           (bind* (g0 s) g ...)))))))
 
(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))
 
(define bind
  (lambda (a-inf g)
    (case-inf a-inf
      (() (mzero))
      ((f) (inc (bind (f) g)))
      ((a) (g a))
      ((a f) (mplus (g a) (lambdaf@ () (bind (f) g)))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s) 
       (inc 
         (mplus* 
           (bind* (g0 s) g ...)
           (bind* (g1 s) g^ ...) ...))))))
 
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
     (lambdag@ (s)
       (inc
         (ifa ((g0 s) g ...)
              ((g1 s) g^ ...) ...))))))
 
(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifa b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* a-inf g ...)))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s)
       (inc
         (ifu ((g0 s) g ...)
              ((g1 s) g^ ...) ...))))))
 
(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifu b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* (unit a) g ...)))))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambdag@ (s)
       (let ((x (walk* x s)) ...)
         ((fresh () g g* ...) s))))))

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
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; O(1) complexity.
(define (adjoin-set x set) (cons x set))

(define-syntax def-asp-rule
    (syntax-rules ()
      ((_ (name args ...) exp ...)
       (begin
         ; [ToDo] Add rule to tracking set.
         ;(display `name)
         ; [ToDo] Define a transformed rule.
         (define name (lambda (args ...)
           ; [ToDo] Mark rule has been executed.
           ;(display `name)
           ; Coinduction and tabling.
           (let ((argv (list args ...))
                  (alt_name (string-append "not_" 
                            (symbol->string `name))))
             (lambdag@ (c : S N F)
             ; Inspect calling stack.
             ;(display S)
             (let ((key (map (lambda (arg)
                                (walk arg S)) argv) ))
               (if (element-of-set? (list (string->symbol alt_name) key) F)
                   (mzero)
                   ; Expand calling stack.
                   ((fresh () exp ...) (list S N (adjoin-set (list (string->symbol alt_name) key) F))))
             )
             ; Positive loop (Tabling)
             ; Negative loop (Even co-inductive success)
             ; Negative loop (Odd co-inductive failure)
             ))
         ))

       ))))

(define-syntax def-asp-complement-rule
    (syntax-rules ()
      ((_ (name args ...) exp ...)
       (begin
         ; [ToDo] Add rule to tracking set.
         ;(display `name)
         ; [ToDo] Define a transformed rule.
         (define name (lambda (args ...)
           ; [ToDo] Mark rule has been executed.
           ;(display `name)
           ; Coinduction and tabling.
           (let ((argv (list args ...)))
             (lambdag@ (c : S N F)
             ; Inspect calling stack.
             ;(display S)
             (let ((key (map (lambda (arg)
                                (walk arg S)) argv) ))
               (if (element-of-set? (list `name key) F)
                   (mzero)
                   ; Expand calling stack.
                   ((fresh () exp ...) (list S N (adjoin-set (list `name key) F))))
             )
             ; Positive loop (Tabling)
             ; Negative loop (Even co-inductive success)
             ; Negative loop (Odd co-inductive failure)
             ))
         ))

       ))))

;; A negation (no) is a procedure that
;(define no
;  ; takes in a goal
;  (lambda (goal)
;    ; and a sequence of subsitution, produces the new subsitution as follows
;    (lambdag@ (c : S N F)
;      ; Add the odd/even negation counter.
;      (let ((newN (+ 1 N)))
;      ; 0/1 means there are even/odd number of negations. 
;        (let ((result (take 1 (lambdaf@() (goal (list S newN F))))))
;            (display c)
;            (newline)
;            ; Negation as failure.
;            (if (null? result)
;              ;Succeed if we failed to prove the goal.
;              (unit c)
;              ; Fail if we found at least one goal.
;              (mzero)
;            )
;        )
;      )
;      ;(let ((newN (- 1 (car N))))
;      ;  ; Compute the goal to see if we can find out at least one proof/subsitution.
;      ;  ; The assumption here is (take) has finite steps, so that it will terminate eventually.
;      ;  (let ((result (take 1 (lambdaf@() (goal (list S (cons newN N) F))))))
;      ;    ; Negation as failure.
;      ;    (if (null? result)
;      ;        ; Succeed if we failed to prove the goal.
;      ;        (unit (list S N '(()) ))
;      ;        (let ((F (c->F (car result)))
;      ;              (nS (c->S (car result))))
;      ;          ; Fail if we found at least one goal.
;      ;          (cond ((and (not (null? nS)) (= (length N) 1))
;      ;                     ; Fail if we found at least one goal.
;      ;                     (mzero))
;      ;                ((not (null? F))
;      ;                 (unit (list (car F) N (list nS))))
;      ;                ((not (null? nS))
;      ;                 (unit (list '() N `(,nS) )))
;      ;                ((null? nS)
;      ;                 (unit (list (car F) N '(()) )))
;      ;                (else (error "unexpected result" result)))
;      ;        )
;      ;    )))
;    )
;  ))

(define-syntax no
    (syntax-rules ()
      ((no (name args ...))
        (let ((alt_name (string-append "not_" 
                        (symbol->string 'name))))
          (lambdag@ (c : S N F)
            (let ((newN (+ 1 N)))
              (let ((newC (list S newN F)))
                (display newN)
                (if (even? newN)
                    ((name args ...) newC)
                    (begin (display alt_name)
                           (newline)
                         (((eval (string->symbol alt_name)) args ...) newC))
                )
              )
            )
          )))))
