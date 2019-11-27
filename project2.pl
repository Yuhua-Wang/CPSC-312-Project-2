:- dynamic a/0,b/0,c1/0,c2/0,c3/0.

% connected(X,Y,C)
% is true if the robot can move from location X to location Y with a cost of C

% the following is defined for Map I on google doc
% e.g. robots can go from a to c1 with a cost of 2:
connected(a, c1, 2).
connected(c1, a, 3).
connected(c1, b, 1).
connected(b, c1, 2).
connected(b, c3, 1).
connected(c3, b, 3).
connected(c1, c2, 4).
connected(c2, c1, 2).

/*
% tests written for shortestPath
connected(j,k,5).
connected(k,l,5).
connected(i,o,10).
connected(o,l,10).
connected(j,i,1).
*/

% order(C,Q,R,U)
% is true if a customer at location C has ordered Q units of food from restaurants at R, and the urgency level is U.

% the following is defined for Map I
% e.g. customer at c3 has ordered 2 units of food from A. This order is urgent.
order(c3, 2, a, urgent).
order(c2, 1, b, not_urgent).
order(c1, 1, b, not_urgent).


% path(From,To,Visited,Cost,Path)
% is true if Path is a list of locations representing a valid path from From to To with the cost being Cost. Visited is a list of visited nodes.
% it is recommend to use findpath(F,T,C,P) as the visited list can be misleading
path(X,X,_,0,[X]).
path(F,T,_ , C, [F,T]) :- dif(F,T), connected(F,T,C).
path(F,T,V,C,[F|P1]) :-
                  dif(F,T), \+connected(F,T,_),
                  connected(F,Z,C1),\+ member(Z,V), path(Z,T,[Z|V],C2,P1), \+ member(F,P1),
                  C is C1+C2.


% findpath(From,To,Cost,Path)
% is true if Path is a list of locations representing a valid path from From to To with cost Cost.
% this is basically the same as path(F,T,V,C,P) but does not show the useless and potentially misleading visited list
findpath(F,T,C,P) :- path(F,T,[F],C,P).


% shortestPath(From,To,Cost,Path)
% is true if Path is a list of location representing the shortest (cheapest) path from From to To with the cost being Cost
shortestPath(F,T,C,P) :- findpath(F,T,C,P), \+notShortest(F,T,C,P).

% notShortest(From,To,Cost,Path)
% is true if there is other path from From to To with lower cost than Cost
notShortest(F,T,C,P) :- findpath(F,T,C1,P1), dif(P,P1), C1<C.


% sorturgent(L1,L2):
% true if L1 is a list of order and L2 conclude all element in L1 in correct sequence(emerge->not emerge).
sorturgent([],[]).
sorturgent([order(P1,X,P2,urgent)|T1],[order(P1,X,P2,urgent)|T2]):- sorturgent(T1,T2).
sorturgent([order(P1,X,P2,not_urgent)|T1],R2):-
                     append(T2,[order(P1,X,P2,not_urgent)],R2), sorturgent(T1,T2).


% append(L1,L2,L3).
% is true if L3 is the list made by appending L1 and L2
append(L1,[],L1).
append([],L2,L2).
append([H1|T],L2,[H1|R]):- append(T,L2,R).


% norepeat(L1, L2).
% true if L2 is the same as L1 without duplicates
norepeat([], []).
norepeat([H|T1],L2) :- member(H,T1), norepeat(T1,L2).
norepeat([H|T1],[H|T2]):- \+member(H,T1), norepeat(T1,T2).

reachable([],[]).
reachable([order(P1,X,P2,urgent)|T1],[order(P1,X,P2,urgent)|T2]):-
findpath(P1,P2,C,P),
reachable(T1,T2).
reachable([order(P1,X,P2,not_urgent)|T1],[order(P1,X,P2,not_urgent)|T2]):-
findpath(P1,P2,C,P),
reachable(T1,T2).


check([],[]).
check(L1,L4):-
norepeat(L1,L2),
reachable(L2,L3),
sorturgent(L3,L4).


% plan(L2,C,P1).
% true if P1 gives the path that the robot can finish all the order.
plan([],0,[]).
plan([order(P1,_,P2,_)|L2],C,P30):-
findpath(P1,P2,C1,P10),
plan(L2,C2,P20),
C is C1+C2,
append(P10,P20,P30).


do([],[]).
do(L1,C,P1):-
check(L1,L2),
plan(L2,C,P1).

%try
%do([order(a,4,c1,urgent)],C,P1).
%do([order(a,4,c1,urgent),order(a,2,c2,urgent)],C,P1).
%do([order(a,4,c1,not_urgent),order(a,2,c3,not_urgent),order(a,4,b,urgent),order(a,4,b,urgent)],C,P1).
%do([order(a,4,c5,urgent)],C,P1).
%do([order(a,4,b,urgent),order(a,4,c,urgent),order(a,4,b,urgent),order(a,4,b,urgent),order(a,4,b,urgent),order(a,4,b,urgent),order(a,4,b,urgent)],P1).

