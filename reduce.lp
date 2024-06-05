reduce(a,b).
reduce(b,c).
reduce(c,d).
reduce(d,e).
reduce(e,c).
reduce(a,f).
reduce(f,g).
reduce(g,f).
reduce(g,k).
reduce(f,h).
reduce(h,i).
reduce(i,h).


reachable(X,Y) :- reduce(X,Y).
reachable(X,Y) :- reachable(X,Z), reduce(Z,Y).


reducible(X) :- reachable(X,Y), not reachable(Y,X).

fullyReduce(X,Y) :- reachable(X,Y), not reducible(Y).

% #show fullyReduce/2.
#show reducible/1.

% win(X) :- reduce(X, Y), not win(Y).

% #show win/1.

% #show fullyRe(a, Y) : fullyReduce(a, Y).
% % #show r/2.
% % #show r(a, Y) : reachable(a, Y).
