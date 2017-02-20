:- op(500, xfy, user:(+)).
:- op(400, xfy, user:(*)).
:- op(500, xfy, user:(-)).
:- op(400, xfy, user:(/)).



simplify(Expr, Ans) :-
  removeSub(Expr, Term0),
  unparse(Term0, ExprNew),
  split(ExprNew, Syms0, Nums0),
  sumlist(Nums0, Nums),
  append([Nums],Syms0 , Syms),
  unparse(Syms, Simplified),
  rules(Simplified,Ans).
  
/*simplify(ExprP, Ans) :-
    removeSub(ExprP, Term0),
    unparse(Term0, Expr),
    removebrackets(Expr, Expr1, Expr2, Expr3),
    simplify(Expr2,Better),
    simplify(Expr1*Better*Expr3,Ans).
    
removebrackets(A*(B)*C,A,B,C):-!.

 simplify(ExprP, Ans) :-
    removeSub(ExprP, Term0),
    unparse(Term0, Expr),
    removebrackets1(Expr, Expr1, Expr2, Expr3),
    simplify(Expr2,Better),
    simplify(Expr1+Better*Expr3,Ans).
    
removebrackets1(A+(B)*C,A,B,C):-!.

simplify(ExprP, Ans) :-
    removeSub(ExprP, Term0),
    unparse(Term0, Expr),
    removebrackets2(Expr, Expr1, Expr2, Expr3),
    simplify(Expr2,Better),
    simplify(Expr1*Better+Expr3,Ans).
    
removebrackets2(A*(B)+C,A,B,C):-!.

*/


split(X+Y, Syms, [X|Nums]) :- integer(X), split(Y, Syms, Nums).
split(X+Y, [X|Syms], Nums) :- atom(X), split(Y, Syms, Nums).
split(X+Y, Syms, [P|Nums]) :- term(X,P),integer(P), split(Y, Syms, Nums).
split(X+Y, [P|Syms], Nums) :- term(X,P), split(Y, Syms, Nums).
split(X, [], [X])          :- integer(X).
split(X, [X], [])          :- atom(X).
split(X, [], [P])          :- term(X,P), integer(P).
split(X, [P], [])          :- term(X,P).

unparse([X], X).
unparse([X|Xs], X+Y)       :- unparse(Xs, Y).

term(X,Solve) :- 
    splitp(X, Syms1, Nums1),
    product(Nums1, Num),
    append(Syms1,[Num],Syms),
    termtogether(Syms, Solve).
    
splitp(A*B, [A|Sym], Num):- atom(A),
splitp(B, Sym, Num).
splitp(A*B, Sym, [A|Num]) :- integer(A), splitp(B, Sym, Num).
splitp(A, [A], [])          :- atom(A).
splitp(A, [], [A])          :- integer(A).

termtogether([A], A).
termtogether([A|As], A*B)       :- termtogether(As, B).

product([X|Nums1], Num) :-
    product(Nums1, Pro),
    Num is X*Pro.

product([X], Num) :-
	Num is X.
    
removeSub(X + Y, [X|Terms]) :- removeSub(Y, Terms).
removeSub(X - Y, [X|Terms]) :- /* Do Something*/addSub(Y, Terms).
removeSub(X, [X]) :- !.

addSub(Y+Z,[Val|Terms]):- not(atom(Y)),Val is Y*(-1), integer(Val), removeSub(Z, Terms).
addSub(Y-Z,[Val|Terms]):- not(atom(Y)),Val is Y*(-1), integer(Val), removeSub(Z, Terms).
addSub(Y+Z,[P|Terms]):- termN(Y,P), removeSub(Z, Terms).
addSub(Y-Z,[P|Terms]):- termN(Y,P), addSub(Z, Terms).
addSub(Y,[P]):- termN(Y,P).

termN(X,Solve) :- 
    splitp(X, Syms1, Nums1),
    productN(Nums1, Num),
    append(Syms1,[Num],Syms),
    termtogether(Syms, Solve).

termN(X,Solve) :- 
    splitp(X, Syms1, Nums1),
    length(Nums1, Len),
    Len == 0,
    append(Syms1,[-1],Syms),
    termtogether(Syms, Solve).


productN([X|Nums1], Num) :-
    product(Nums1, Pro),
    Num is X*Pro.

productN([X], Num) :-
	Num is X*(-1).


simplify(Expr,Ans) :- rules(Expr, Ans).
rules(A+B, C) :- !, rules(A, A1), rules(B, B1), identity(A1+B1, C).
rules(A-B, C) :- !, rules(A, A1), rules(B, B1), identity(A1-B1, C).
rules(A/B, C) :- !, rules(A, A1), rules(B, B1), identity(A1/B1, C).
rules(A*B, C) :- rules(A, A1), rules(B, B1), identity(A1*B1, C).

identity(A/B, C) :- integer(A), integer(B), !,C is A/B.
identity(A*B, C) :- float(A), float(B), !,C is A*B.
identity(A*B, C) :- float(A), integer(B), !,C is A*B.
identity(A*B, C) :- integer(A), float(B), !,C is A*B.


identity(0+A, A) :- !.
identity(A+0, A) :- !.
identity(A*0, 0) :- !.
identity(0*A, 0) :- !.
identity(A+ -A, 0) :- !.
identity(A*B/B,A) :- !.
identity(A-A,0) :- !.
identity(A+B-A,B):-!.
identity(A+A,2*A):-!.
identity(A/A,1):-!.
identity(A/1,A):-!.
identity(0/A,0):-!.
identity((B*A)/A,B):-!.
identity(A+B+A,2*A+B):-!.
identity(A*M +b+ A*N, A*Val+b) :-
    integer(M),
    integer(N),
    Val is M+N.
identity(A*M + A*N, A*Val) :-
    integer(M),
    integer(N),
    Val is M+N.

identity(A*M + N*A, A*Val) :-
    integer(M),
    integer(N),
    Val is M+N.
identity(A*M + -A*N, A*Val) :-
    integer(M),
    integer(N),
    Val is M-N.

identity(A*M + -A, A*Val) :-
    integer(M),
    Val is M-1.
identity(M*A + -A, A*Val) :-
    integer(M),
    Val is M-1.
    
identity(A+A*N, A*Val) :-
    integer(N),
    Val is N+1,
    not(Val is 1).

identity(A*N+A, A*Val) :-
    integer(N),
    Val is N+1.

identity(A+b+A*N, A*Val+b) :-
    integer(N),
    Val is N+1,
    not(Val is 1).
identity(A+b+N*A, Val*A+b) :-
    integer(N),
    Val is N+1,
    not(Val is 1).

identity(A+A*N, A) :-
    integer(N),
    Val is N+1,
    Val is 1.
identity(A+A*N, -A) :-
    integer(N),
    Val is N+1,
    Val is -1.
identity(A*M + N* -A, Val*A) :-
    integer(N),
    integer(M),
    Val is M-N.
   
identity(A* -1, -A):- !.
identity(-1*A, -A):- !.
identity(A*1, A) :- !.
identity(1*A, A) :- !.

rules(X, X).
identity(X, X).
simplify(X,X).
