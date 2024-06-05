;% solve the N queens problem for a given N, returning a list of queens as Q
;nqueens(N, Q) :- 
;    nqueens(N, N, [], Q).

;% pick queens one at a time and test against all previous queens
; nqueens(X, N, Qi, Qo) :-
;    X > 0,
;    pickqueen(X, Y, N),
;    not attack(X, Y, Qi),
;    X1 is X - 1,
;    nqueens(X1, N, [q(X, Y) | Qi], Qo).

;nqueens(0, _, Q, Q).

(define (gt lhs rhs)
  (lambdag@ (n f c : S F G)
    (let ((lhs-num (walk lhs S))
          (rhs-num (walk rhs S)))
            (if (> lhs-num rhs-num)
                (succeed n f c)
                (fail n f c))
    )
  )
)

(define (sub minuend subtrahend res)
  (lambdag@ (n f c : S F G)
    (let ((minuend-num (walk* minuend S))
          (subtrahend-num (walk* subtrahend S)))
        ; (cout "minuend: " minuend-num "subtrahend: " subtrahend-num nl)
        ((== res (- minuend-num subtrahend-num)) n f c)
    )
  )
)

(define (queens x n qi qo)
    (conde
        [(== x 0)
         (== qi qo)]
        [(fresh (x1 y qn)
            (gt x 0)
            (pickqueen x y n)
            (no (attack x y qi))
            (sub x 1 x1)
            (== `((,x ,y) . ,qi) qn)
            (queens x1 n qn qo)
        )]
    )
)

(define (nqueens n q)
    (queens n n `() q))

;% pick a queen for row X.

; pickqueen(X, Y, Y) :-
;    Y > 0, q(X, Y).

; pickqueen(X, Y, N) :-
;    N > 1,
;    N1 is N - 1,
;    pickqueen(X, Y, N1).

(define (pickqueen x y n)
    (conde
        [(gt n 0)
         (== y n)]
        [(fresh (n1)
            (gt n 1)
            (sub n 1 n1)
            (pickqueen x y n1))]
    )
)

;% check if a queen can attack any previously selected queen
; attack(X, _, [q(X, _) | _]). % same row
; attack(_, Y, [q(_, Y) | _]). % same col
; attack(X, Y, [q(X2, Y2) | _]) :- % same diagonal
;    Xd is X2 - X, abs(Xd, Xd2),
;    Yd is Y2 - Y, abs(Yd, Yd2),
;    Xd2 = Yd2.

; attack(X, Y, [_ | T]) :-
;    attack(X, Y, T).

(define (diagonal x y x1 y1)
  (lambdag@ (n f c : S F G)
    (let ((x-num (walk* x S))
          (y-num (walk* y S))
          (x1-num (walk* x1 S))
          (y1-num (walk* y1 S)))
        (if (and (number? x-num) (number? y-num) (number? x1-num) (number? y1-num))
            (if (= (abs (- x-num x1-num)) (abs (- y-num y1-num)))
                (succeed n f c)
                (fail n f c))
            (fail n f c)))
  ))


(def-asp-rule (attack x y qs)
    (conde
        [(fresh (h t)
            (== `(,h . ,t) qs)
            (attack x y t)
          )]
        [(fresh (x1 y1 t)
            (== `((,x1 ,y1) . ,t) qs)
            (== x1 x))]
        [(fresh (x1 y1 t)
            (== `((,x1 ,y1) . ,t) qs)
            (== y1 y))]
        [(fresh (x1 y1 t)
            (== `((,x1 ,y1) . ,t) qs)
            (diagonal x y x1 y1)
            )]
    )
)

;q(X, Y) :- not negq(X, Y).
;negq(X, Y) :- not q(X, Y).

;abs(X, X) :- X >= 0.

;abs(X, Y) :- X < 0, Y is X * -1.

;(run 1 (q) (fresh (x1 y1 x2 y2) (queen x1 y1) (queen x2 y2) (no (== `(,x1 ,y1) 
;`(,x2 ,y2))) (== q `(queen(,x1 ,y1) queen(,x2 ,y2)))))

