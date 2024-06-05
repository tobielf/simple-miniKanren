(load "test-check.scm")
; The comment section is corresponding Clingo code.
; node(a).
; node(b).
; node(c).
; node(d).

(def-asp-rule (node x)
    (conde
      [(== x 'a)]
      [(== x 'b)]
      [(== x 'c)]
      [(== x 'd)]
      ;[(== x 'e)]
      ;[(== x 'f)]
      ;[(== x 'g)]
      ;[(== x 'h)]
      ;[(== x 'i)]
      ;[(== x 'j)]
    ))

; edge(a, b).
; edge(b, a).
; edge(b, c).
; edge(c, d).

(def-asp-rule (edge x y)
    (conde
      [(== x 'b)
       (== y 'c)]
      [(== x 'a)
       (== y 'b)]
      [(== x 'b)
       (== y 'a)]
      [(== x 'c)
       (== y 'd)]))

(def-asp-rule (win x)
  (conde
  [(fresh (y)
    (edge x y)
    (no (win y)))]))


(def-asp-rule (reachable x y)
  (conde
    [(edge x y)]
  ;  [(conde 
       [(edge x 'a)
        (reachable 'a y)]
       [(edge x 'b)
        (reachable 'b y)]
       [(edge x 'c)
        (reachable 'c y)]
       [(edge x 'd)
        (reachable 'd y)]
  ;      )]
  ))

(def-asp-rule (reachable x y)
  (conde
    [(edge x y)]
    [(fresh (z) (edge x z) (reachable z y))]
  )
)

(define (co_reachable x y)
  (fresh ()
  (conde-t
    [(edge x y)]
  ;  [(conde 
       [(conde 
          [(edge x 'a)]
          [;(no (edge x 'a)) 
           (reachable 'a y)])]
       [(conde
          [(edge x 'b)]
          [;(no (edge x 'b))
           (reachable 'b y)])]
       [(conde
          [(edge x 'c)]
          [;(no (edge x 'c))
           (reachable 'c y)])]
       [(conde
          [(edge x 'd)]
          [;(no (edge x 'd))
           (reachable 'd y)])]
  ;      )]
  )
  (update-F `reachable (list x y)))
)

(def-asp-rule (unreachable x y)
  (conde
    [(no (reachable x y))]))

(define (co_unreachable x y)
  (conde-t
    [(no (reachable x y))])
  (update-F `unreachable (list x y))
)

; reachable(X, Y) :- edge(X, Y).
; reachable(X, Y) :- edge(X, Z), reachable(Z, Y).

(define reachable
  (lambda (x y)
    (conde
      [(edge x y)]
      [(fresh (z)
        (edge x z)
        (reachable z y))])))

; unreachable(X, Y) :- node(X), node(Y), not reachable(X, Y).

(define unreachable
  (lambda (x y)
    (conde
      [(node x)
       (node y)
       (no (reachable x y))])
  ))

; Original predicate version.
(def-asp-rule (win x)
  (conde
  [(fresh (y)
    (edge x y)
    (no (win y)))]))

; Grounded propositional version.
(def-asp-rule (wina)
  (conde
  [(edge 'a 'b)
   (no (winb))]
  [(edge 'a 'c)
   (no (winc))]
  [(edge 'a 'd)
   (no (wind))]))

(def-asp-rule (winb)
  (conde
  [(edge 'b 'a)
   (no (wina))]
  [(edge 'b 'c)
   (no (winc))]
  [(edge 'b 'd)
   (no (wind))]))

(def-asp-rule (winc)
  (conde
  [(edge 'c 'b)
   (no (winb))]
  [(edge 'c 'a)
   (no (wina))]
  [(edge 'c 'd)
   (no (wind))]))

(def-asp-rule (wind)
  (conde
  [(edge 'd 'b)
   (no (winb))]
  [(edge 'd 'c)
   (no (winc))]
  [(edge 'd 'a)
   (no (wina))]))

; Expanded predicate version.
(def-asp-rule (win x)
  (conde
    [(edge x 'a)
     (no (win 'a))]
    [(edge x 'b)
     (no (win 'b))]
    [(edge x 'c)
     (no (win 'c))]
    [(edge x 'd)
     (no (win 'd))]
  )
)

; goal(X) :- node(X), not sub_goal1(X).
; goal(X) :- node(X), sub_goal2(X).

(define goal
  (lambda (x)
    (conde
      [(no (sub_goal1 x))]
      [(sub_goal2 x)])))

; sub_goal1(X) :- node(X), X = a.
; sub_goal1(X) :- node(X), X != b.

(define sub_goal1
  (lambda (x)
    (conde
      [(== x 'a)]
      [(no (== x 'b))])))

; sub_goal2(X) :- node(X), node(Y), not edge(X, Y).

(define sub_goal2
  (lambda (x)
    (fresh (y)
      (node y)
      (no (edge x y)))))

; sub_goal2(X) :- node(X), node(Y), edge(X, Y).
(define sub_goal2
  (lambda (x)
    (fresh (y)
      (node y)
      (edge x y))))


;delete(X,Y) :- edge(X,Y), not keep(X,Y).
;keep(X,Y) :- edge(X,Y), delete(X1,Y1), X1 != X.
;keep(X,Y) :- edge(X,Y), delete(X1,Y1), Y1 != Y.
;reachable(X,Y) :- keep(X,Y).
;reachable(X,82) :- reachable(X,Z),reachable(Z,82).
;edge(1,1).
;edge(1,2).
;edge(1,5).

(define delete
  (tabled (x y)
    (edge x y)
    (no (keep x y)))
)

(define keep
  (tabled (x y)
    (conde
      [(fresh (x1 y1)
       (edge x y)
       (delete x1 y1)
       (no (== x1 x)))]
      [(fresh (x1 y1)
       (edge x y)
       (delete x1 y1)
       (no (== y1 y)))])))

(define edge
  (lambda (x y)
    (conde
      [(== x 1)
       (== y 1)]
      [(== x 1)
       (== y 2)]
      [(== x 1)
       (== y 5)])))

;node(a).
;node(b).
;node(c).
;node(d).
(def-asp-rule (node x)
  (conde
      [(== x 'a)]
      [(== x 'b)]
      [(== x 'c)]
      [(== x 'd)]))
;move(a, b).
;move(b, a).
;move(b, c).
;move(c, d).
(def-asp-rule (move x y)
  (conde
      [(== x 'b)
       (== y 'a)]
      [(== x 'a)
       (== y 'b)]
      [(== x 'b)
       (== y 'c)]
      [(== x 'c)
       (== y 'd)]))

(def-asp-rule (nomove x y)
  (conde [
    (node x)
    (node y)
    (no (move x y))]))

(def-asp-rule (dummy x y)
  (conde [(nomove x y)]))

;win(X) :- node(X), node(Y), move(X, Y), not win(Y).
(def-asp-rule (win x)
  (conde 
    [(fresh (y)
      (node x)
      (node y)
      (edge x y)
      (no (win y)))]))

(define win
  (tabled (x)
  (fresh (y)
    (node x)
    (node y)
    (move x y)
    (no (win y)))))

; Completion
; s(a).
; s(b).
(define s
  (lambda (x)
    (conde
      [(== x 'a)]
      [(== x 'b)])))

; t(a, 1).
(define t
  (lambda (x y)
    (conde
      [(== x 'a)
       (== y 1)])))

; m(1).
(define m
  (lambda (x)
    (conde
      [(== x 1)])))

; non_math(X) :- s(X), m(Y), not t(X, Y).
(define non_math
  (lambda (x)
    (fresh (y)
      (s x)
      (m y)
      (no (t x y)))))

; a(1).
(def-asp-rule (a x)
  (conde
    [(== x 'e)]))

; b(2).
(def-asp-rule (b x)
  (conde
    [(== x 'f)]
    [(== x 'e)]
    ;[(== x 1)]
  ))

; p(X) :- a(X), not p(X).
; p(X) :- b(X).
(def-asp-rule (p x)
  (conde
    [(a x)
     (no (p x))]
    [(b x)]
  ))

;(def-asp-rule (nmr_p x)
;  (conde
;    [(a x)
;     (no (p x))]
;    [fail]
;  ))



(def-asp-rule (a1)
    (succeed)
)

; a1.
(def-asp-rule (a1)
  (conde
    [succeed]
  )
)
; b2.
(def-asp-rule (b2)
  (conde
    [succeed]
  )
)

; a2
(def-asp-rule (a2)
  (conde
    [fail]
  )
)
; b1.
(def-asp-rule (b1)
  (conde
    [fail]
  )
)
; p1 :- a1, not p1.
; p1 :- b1.
(def-asp-rule (p1)
  (conde
    [(a1) (no (p1))]
    [(b1)])
)
; p2 :- a2, not p2.
; p2 :- b2.
(def-asp-rule (p2)
  (conde
    [(a2) (no (p2))]
    [(b2)])
)

; a.
(def-asp-rule (a)
  (conde
    [(== #f #f)]))
; b.
(def-asp-rule (b)
  (conde
    [(== #f #f)]))
; p:- a, not p.
(def-asp-rule (p)
  (conde
    [(a)
     (no (p))]
    [(b)]
  ))

;a(1).
;r(2).
;p(X) :- a(X), not r(X).
;q(X) :- a(X), not p(X).
;s(X) :- a(X), not q(X).
;a(1).
(def-asp-rule (a x)
  (conde 
    [(== x 1)]))
(def-asp-rule (r x)
  (conde
    [(== x 2)]))
(def-asp-rule (p x)
  (conde
    [(a x) (no (r x))]))
(def-asp-rule (q x)
  (conde
    [(a x) (no (p x))]))
(def-asp-rule (s x)
  (conde
    [(a x) (no (q x))]))


;a :- not b.
;b :- not c.
;c :- not a.

;c :- not d.
;d :- not c.
(def-asp-rule (a)
  (conde 
   [(no (b))]
))
(def-asp-rule (b)
  (conde
   [(no (c))]
   [(no (e))]
))
(def-asp-rule (c)
  (conde
   [(no (a))]
   [(no (d))]
))
(def-asp-rule (d)
  (conde
   [(no (c))]
))

(def-asp-rule (e)
  (conde
   [(no (b))]
))

;b :- not e.
;e :- not b.


;a :- b.
(def-asp-rule (a)
  (conde 
    [(b)]
    [(c)]))
;b :- a.
(def-asp-rule (b)
  (conde
    [(a)]))
;a :- c.
;c :- b.
(def-asp-rule (c)
  (conde
    [(b)]))

;p :- not a.
(def-asp-rule (p)
  (conde
    [(no (a))]))

;q :- not p.
(def-asp-rule (q)
  (conde
    [(no (p))]))



;p :- a, r, not p.
(def-asp-rule (p)
  (conde
   [(a)
    (r)
    (no (p))]
   ;[(b)]
  )
)
;r :- not q.
(def-asp-rule (r)
  (conde
   [(no (q))]
  )
)
;q :- not r.
(def-asp-rule (q)
  (conde
   [(no (r))]
  )
)
;a.
(def-asp-rule (a)
  (conde
   [succeed]
  )
)
;b.
(def-asp-rule (b)
  (conde
   [succeed]
  )
)


; Stratified Negation

;reduce(a,b).
;reduce(b,c).
;reduce(c,d).
;reduce(d,e).
;reduce(e,c).
;reduce(a,f).
;reduce(f,g).
;reduce(g,f).
;reduce(g,k).
;reduce(f,h).
;reduce(h,i).
;reduce(i,h).
(def-asp-rule (reduce x y)
  (conde
    [(== x 'a) (== y 'b)]
    [(== x 'b) (== y 'c)]
    [(== x 'c) (== y 'd)]
    [(== x 'd) (== y 'e)]
    [(== x 'e) (== y 'c)]
    [(== x 'a) (== y 'f)]
    [(== x 'f) (== y 'g)]
    [(== x 'g) (== y 'f)]
    [(== x 'g) (== y 'k)]
    [(== x 'f) (== y 'h)]
    [(== x 'h) (== y 'i)]
    [(== x 'i) (== y 'h)]
  ))

;reachable(X,Y) :- reduce(X,Y).
;reachable(X,Y) :- reachable(X,Z), reduce(Z,Y).
(def-asp-rule (reachable x y)
  (conde
    [(reduce x y)]
    [(fresh (z) (reduce x z) (reachable z y))]
  ))
;reducible(X) :- reachable(X,Y), not reachable(Y,X).
(def-asp-rule (reducible x)
  (conde
   [(fresh (y) (reachable x y) (no (reachable y x)))]
  )
)

;fullyReduce(X,Y) :- reachable(X,Y), not reducible(Y).
(def-asp-rule (fullyReduce x y)
  (conde
    [(reachable x y) (no (reducible y))]
  )
)

(def-asp-complement-rule (co_reduce x y)
  (fresh ()
    (conde [(== x 'a)] [(no (== x 'a)) (=/= y 'b)])
    (conde [(== x 'b)] [(no (== x 'b)) (=/= y 'c)])
    (conde [(== x 'c)] [(no (== x 'c)) (=/= y 'd)])
    (conde [(== x 'd)] [(no (== x 'd)) (=/= y 'e)])
    (conde [(== x 'e)] [(no (== x 'e)) (=/= y 'c)])
    (conde [(== x 'a)] [(no (== x 'a)) (=/= y 'f)])
    (conde [(== x 'f)] [(no (== x 'f)) (=/= y 'g)])
    (conde [(== x 'g)] [(no (== x 'g)) (=/= y 'f)])
    (conde [(== x 'g)] [(no (== x 'g)) (=/= y 'k)])
    (conde [(== x 'f)] [(no (== x 'f)) (=/= y 'h)])
    (conde [(== x 'h)] [(no (== x 'h)) (=/= y 'i)])
    (conde [(== x 'i)] [(no (== x 'i)) (=/= y 'h)])
  ))

(def-asp-complement-rule (co_reachable x y)
  (reduce x y)
  (fresh (z) (conde [(reduce x z)] [(no (reduce x z)) (reachable z y)]))
)

(def-asp-complement-rule (co_reducible x)
  (fresh (y) (conde [(reachable x y)] [(no (reachable x y)) (no (reachable y x))])))


(define (unbound-var-checking variables)
  (if (null? variables)
      #t
      (if (not (var? (car variables)))
        #f
        (unbound-var-checking (cdr variables)))))

(define-syntax check-variables
  (syntax-rules ()
    ((_ args ...)
      (lambdag@ (n f c : S F G)
        (let ((argv (list args ...)))
          (let ((key (map (lambda (arg)
                              (walk arg S)) argv) ))
            (cout "--------" nl)
            (cout "argv:" argv nl)
            (cout "key:" key nl)
            (cout "frame: " f nl)
            (cout "S:" S nl)
            ;(cout "var?:" (var? (car key)) nl)
            ;(cout "n:" n nl)
            (cout "G:" G nl)
            (cout "========" nl)
            (if (unbound-var-checking key)
              (unit n f (list S F G))
              (unit n f (list S F G))
              ;(mzero) ;check all other value been bounded to this variable.
            )
          )
        )
      )
    )
  )
)
