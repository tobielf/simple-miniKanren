(load "mk.scm")

(def-asp-rule (reduce x y)
  (conde
    [(== x 'a) (== y 'b)]
    [(== x 'b) (== y 'c)]
    [(== x 'c) (== y 'd)]
    [(== x 'd) (== y 'e)]
    [(== x 'e) (== y 'c)]
    [(== x 'a) (== y 'f)]
    [(== x 'f) (== y 'h)]
    [(== x 'f) (== y 'g)]
    [(== x 'g) (== y 'f)]
    [(== x 'g) (== y 'k)]
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
