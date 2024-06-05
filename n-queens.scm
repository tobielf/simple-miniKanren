(def-asp-rule (num x)
 (conde
   [(== x 'a)]
   [(== x 'b)]
   [(== x 'c)]
   [(== x 'd)]
   ;[(== x 'e)]
   ;[(== x 'f)]
   ;[(== x 'g)]
   ;[(== x 'h)]
))

; queen(X,Y) :- not free(X,Y), num(X), num(Y).
(def-asp-rule (queen x y)
 (conde
   [(num x) (num y) (no (free x y))]
 ))

; free(X,Y) :- not queen(X,Y), num(X), num(Y).
(def-asp-rule (free x y)
 (conde
   [(num x) (num y) (no (queen x y))]
 ))

; row(X) :- queen(X,Y).
(def-asp-rule (row x)
 (conde
   [(fresh (y)
      (queen x y))]
 ))
; col(Y) :- queen(X,Y).
(def-asp-rule (col y)
 (conde
   [(fresh (x)
      (queen x y))]
 ))

;fail :- num(X), not row(X), not fail.
;fail :- num(Y), not col(Y), not fail.

;fail :- queen(X,Y), queen(X,V), Y < V, not fail.
;fail :- queen(X,Y), queen(U,Y), X < U, not fail.

;fail :- queen(X,Y), queen(U,V), X-Y == U-V, f(X,Y) < f(U,V), not fail.
;fail :- queen(X,Y), queen(U,V), X+Y == U+V, f(X,Y) < f(U,V), not fail.
(def-asp-rule (failed)
 (conde
   [(fresh (x)
     (num x) (no (row x)) (no (failed)))]
   [(fresh (y)
     (num y) (no (col y)) (no (failed)))]
   [(fresh (x y v)
     (queen x y)
     (queen x v)
     (no (== y v))
     (no (failed)))]
   [(fresh (x y u)
     (queen x y)
     (queen u y)
     (no (== x u))
     (no (failed)))]
 )
)

 ; :- num(X), not row(X).
 ; :- num(Y), not col(Y).

 ; :- queen(X,Y), queen(X,V), Y < V.
 ; :- queen(X,Y), queen(U,Y), X < U.

 ; :- queen(X,Y), queen(U,V), X-Y == U-V, f(X,Y) < f(U,V).
 ; :- queen(X,Y), queen(U,V), X+Y == U+V, f(X,Y) < f(U,V).
 