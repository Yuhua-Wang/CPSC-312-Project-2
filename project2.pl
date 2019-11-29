:- dynamic a/0,b/0,c1/0,c2/0,c3/0,currentShortestPath/2.

%restaurant(P).
% true if P is a restaurant.
restaurant(a).
restaurant(b).

%customer(P).
% true if P is a customer.
customer(c1).
customer(c2).
customer(c3).
% connected(X,Y,C)
% is true if the robot can move from location X to location Y with a cost of C

% the following is defined for Map I on google doc
% e.g. robots can go from a to c1 with a cost of 2:
connected(a, c1, 2).
connected(c1, a, 2).
connected(c1, b, 1).
connected(b, c1, 1).
connected(b, c3, 3).
connected(c3, b, 3).
connected(c1, c2, 4).
connected(c2, c1, 4).

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
shortestPath(F,T,C,P) :- findpath(F,T,C,P), \+notShortestPath(F,T,C,P).

% notShortestPath(From,To,Cost,Path)
% is true if there is other path from From to To with lower cost than Cost
notShortestPath(F,T,C,P) :- findpath(F,T,C1,P1), dif(P,P1), C1<C.

% currentShortestPath will store current shortest path and cost from start point to end point
% To is the goal node, ReversedPath is a reversed path. C is cost
% currentShortestPath([To|ReversedPath], C)

% edge(From,To,Cost) is true if from From is connected to To directly with distance Cost
edge(From,To,Cost) :- connected(To,From,Cost),
                        connected(From, To, Cost).
shorterPath2([H|Path], Cost) :-		      
	currentShortestPath([H|T], C), Cost < C,        
	retract(currentShortestPath([H|_],_)),
	assert(currentShortestPath([H|Path], Cost)).

shorterPath2(Path, Cost) :-		    
	assert(currentShortestPath(Path,Cost)).
 
goThroughAllNodes(From, Path, Cost) :-		   
    edge(From, T, C),
	\+memberchk(T, Path),	  
	S is C+Cost,
	shorterPath2([T,From|Path], S),
	goThroughAllNodes(T,[From|Path], S).	  
 
goThroughAllNodes(From) :-
	retractall(currentShortestPath(_,_)),        
	goThroughAllNodes(From,[],0).     

goThroughAllNodes(_).
 
getShortestPath(From, To, Cost, Path) :-
	goThroughAllNodes(From),                  
	currentShortestPath([To|ReversedPath], Cost),        
	reverse([To|ReversedPath], Path).   	

printOutShortestPath(From, To) :-                 
	getShortestPath(From, To, Cost, Path)->            
	writef('shortest path is %w with cost %w\n', [Path, Cost]);
	writef('There is no path from %w to %w\n', [From, To]).

%try
% printOutShortestPath(a,b).   will print the shortest path between a and b with a cost if there is such a path.
% if you need to get the shortest path and cost from A to B, please use getShortestPath(A, B, Path, Cost).



% findpdpair(Order, PDPair).
% is true if PDPair pdpair(P,D) represents a pair of pickup(P) and delivery(D) locations of an order pickup and delivery locations
findpdpair(order(D,_,P,_), pdpair(P,D)).


% findAllPdpairs(Orders, PDPairs).
% is true if PDpairs is a list that includes all pdpairs in Orders (a list of orders)
findAllPdpairs([], []).
findAllPdpairs( [order(C,_,R,_)|O] ,[pdpair(R,C)|P]) :- findAllPdpairs(O,P).


% findAllLocations(Orders, Locations).
% true if Locations is a list that contains all locations required to fulfill all orders
findAllLocations([],[]).
findAllLocations([order(C,_,P,_)|O], [C,P|L]) :- findAllLocations(O,L).


% route(PDPairs, Route, From, AllLocations, Visited, Cost).
% is true if Route is a list of locations which represents a valid route to delivery all pdpairs in PDPairs
% From is the starting location
% AllLocations is a list of all locations required to fulfill all orders.
% Visited is the max number of nodes allowed to be visited (cut the route if to many locations have been visited)
% Cost is the cost of the route
% for each pdpair(R,C), R must be reached before C to complete the order

route([pdpair(P,D)],R,F,_,V,C) :- member(P,V), getShortestPath(F,D,C,R).
route([pdpair(P,D)],R,F,_,V,C) :-
                       \+member(P,V), getShortestPath(F,P,C1,R1),getShortestPath(P,D,C2,R2),
                       C is C1+C2, append(R1,R2,R).
route(O,R,F,A,V,C) :-
             dif(F,X), member(X,A), \+member(X,V), getShortestPath(F,X,C1,P),
             removefulfilled(O,V,X,NO), route(NO,R1,X,A,[X|V],C2),
             C is C1+C2, append(P,R1,R).

% try: route([pdpair(a, b), pdpair(b, c3)], R, a, [b, a, c3, b],[a],C).


% findRoute (Orders,Start,Route,Cost).
% is true if Route a list of locations representing a route to fulfill all orders in Orders. Cost is the Cost of the route.
% Start is the starting location.
findRoute(O,S,R,C) :-
             findAllLocations(O,L), findAllPdpairs(O,P),
             route(P,R,S,L,[S],C).

% try: findRoute([order(b,2,a,urgent),order(c3,2,b,urgent)], a, R, C).


% shortestRoute (Orders,Start,Route,Cost).
% is true if Route a list of locations representing the shortest route to fulfill all orders in Orders. Cost is the Cost of the route.
% Start is the starting location.
shortestRoute(O,S,R,C):-
             findRoute(O,S,R,C),\+notShortestRoute(O,S,R,C).

% try: shortestRoute([order(b,2,a,urgent),order(c3,2,b,urgent)], a, R, C).


% notShortestRoute(Orders,Start,Route,Cost)
% is true if there is other path from From to To with lower cost than Cost
notShortestRoute(O,S,R,C) :- findRoute(O,S,R1,C1), dif(R,R1), C1<C.


% removefulfilled(PDPairs, Route, NewLocation, NewPDPairs)
% is true if NewPDPairs is PDPairs but removing any pdpair that is fulfilled by adding the new Location NewLocation into the route Route
% a pdpair(P,D) is fulfilled if P is reached before D
removefulfilled([],_,_,[]).
removefulfilled([pdpair(Y,L)|P], R, L,N) :- member(Y,R), removefulfilled(P,R,L,N).
removefulfilled([pdpair(Y,L)|P], R, L,[pdpair(Y,L)|N]) :- \+ member(Y,R), removefulfilled(P,R,L,N).
removefulfilled([pdpair(A,B)|P], R, L,[pdpair(A,B)|N]) :- dif(B,L), removefulfilled(P,R,L,N).


% append(L1,L2,L3).
% is true if L3 is the list made by appending L1 and L2
append([],[],[]).
append(L1,[],L1).
append([],L2,L2).
append([H1|T],L2,[H1|R]):- append(T,L2,R).

%try
% append([a,b],[c],L).
% append([a,b],[],L).
% append([],[c],L).

% sorturgent(L1,L2):
% true if L1 is a list of order and L2 conclude all element in L1 in correct sequence(emerge->not emerge).
sorturgent([],[]).
sorturgent([order(P1,X,P2,urgent)|T1],[order(P1,X,P2,urgent)|T2]):- sorturgent(T1,T2).
sorturgent([order(P1,X,P2,not_urgent)|T1],R2):-
                     append(T2,[order(P1,X,P2,not_urgent)],R2), sorturgent(T1,T2).


% norepeat(L1, L2).
% true if L2 is the same as L1 without duplicates
norepeat([], []).
norepeat([H|T1],L2) :- member(H,T1), norepeat(T1,L2).
norepeat([H|T1],[H|T2]):- \+member(H,T1), norepeat(T1,T2).

%reachable(L1,L2).
% true if L2 contains all elements in L1 whose receiver place is reachable from the restefrant.

reachable([],[]).
reachable([order(P1,X,P2,urgent)|T1],[order(P1,X,P2,urgent)|T2]):-
findpath(P1,P2,C,P),
reachable(T1,T2).

reachable([order(P1,X,P2,not_urgent)|T1],[order(P1,X,P2,not_urgent)|T2]):-
findpath(P1,P2,C,P),
reachable(T1,T2).


%check(L1,L2).
% true if L2 contains all legal order in L1.
check([],[]).
check(L1,L4):- norepeat(L1,L2), reachable(L2,L3), sorturgent(L3,L4).

%try
%check([order(a,4,c1,urgent),order(a,2,c2,urgent)],L2).
%check([order(c1,4,a,not_urgent),order(c3,2,a,not_urgent),order(c2,4,b,urgent),order(c1,4,b,urgent)],L2).

% plan(L2,C,P1).
% true if P1 gives the path that the robot can finish all the order.
plan([],0,[]).
plan([order(P1,_,P2,_)|L2],C,P30):-
findpath(P1,P2,C1,P10),
plan(L2,C2,P20),
C is C1+C2,
append(P10,P20,P30).

%try
%plan([order(a,4,c1,urgent)],C,P1).

% do(Order,Cost,Path)
% a general Path of Order and its cost are produced.
do([],_,[]).
do(L1,C,P1):-
check(L1,L2),
plan(L2,C,P1).

%try
%do([order(a,4,c1,urgent)],C,P1).
%do([order(a,4,c1,urgent),order(a,2,c2,urgent)],C,P1).
%do([order(c1,4,a,not_urgent),order(c3,2,a,not_urgent),order(c2,4,b,urgent),order(c1,4,b,urgent)],C,P1).
%do([order(a,4,c5,urgent)],C,P1).
%do([order(a,4,b,urgent),order(a,4,c,urgent),order(a,4,b,urgent),order(a,4,b,urgent),order(a,4,b,urgent),order(a,4,b,urgent),order(a,4,b,urgent)],C,P1).


% seperateByUrgent(L1,L2,L3).
% true if the L2 contains all urgent orders in L1 and L3 contains all not_urgent orders in L1 (all orders in L2,L3 as pairs don't have urgency and food).
seperateByUrgent([],[],[]).
seperateByUrgent([order(P1,X,P2,urgent)|T1],[(P1,P2)|T2],L3):- seperateByUrgent(T1,T2,L3).
seperateByUrgent([order(P1,X,P2,not_urgent)|T1],L2,[(P1,P2)|T3]):- seperateByUrgent(T1,L2,T3).

%try
% seperateByUrgent([order(a,4,c1,not_urgent),order(a,4,c1,urgent),order(a,4,c2,urgent)],L2,L3).
% seperateByUrgent([order(a,4,c1,urgent),order(a,4,c2,urgent)],L2,L3).

% seperateByUrgent([order(c2, 4, b, urgent), order(c1, 4, b, urgent), order(c3, 2, a, not_urgent), order(c1, 4, a, not_urgent)] ,L2,L3).

% nearest(F1,L1,P2,L2,C).
% find the P1's nearest place in L1,cost C produce as P2 and rest of L1 is L2.
nearest(_,[],empty,1000000).
nearest(empty,[],empty,1000000).

nearest(F1,[P2|T],P3,C1):-
\+ dif(F1,P2),
nearest(F1,T,P3,C1).

nearest(F1,[P2|T],P2,C):-
dif(F1,P2),
shortestPath(F1,P2,C,Path1),
nearest(F1,T,P3,C1),
C<C1.

nearest(F1,[P2|T],P2,C):-
dif(F1,P2),
shortestPath(F1,P2,C,Path1),
nearest(F1,T,P3,C1),
C=C1.

nearest(F1,[P2|T1],P3,C1):-
dif(F1,P2),
shortestPath(F1,P2,C,Path1),
nearest(F1,T1,P3,C1),
C>C1.

%try
%nearest(a,[a,b,b],P1,C).
%nearest(a,[b,a,c1],P1,C).
%nearest(a,[b,b,b,b,b],P1,C).
%nearest(a,[a],P1,C).

%seperateR(L1,L2).
% find all restaurant in L1.
seperateR([],[]).
seperateR([(P1,P2)|T1],[P2|T2]):-
seperateR(T1,T2).

%try
%seperateR([(a,c1)],L2).
%seperateR([(c1,a),(c2,b)],L2).
%seperateR([(c2, b),  (c1, b)],L).
%seperateR([(c3, a),  (c1, a)],L).

%findReceiver(R1,L1,C1,L2).
% find the receiver of restaurant R1 in the order pairs and drop that pair from the list of order to make L2.
findReceiver(_,[],empty,[]).
findReceiver(empty,[],empty,[]).

findReceiver(R1,[(C2,R2)|T1],C3,[(C2,R2)|T2]):-
dif(R1,R2),
findReceiver(R1,T1,C3,T2).

findReceiver(R1,[(C1,R1)|T1],C1,T1).

%try
%findReceiver(a,[],C1,T1).
%findReceiver(a,[(c1,a),(c2,b)],C1,T1).
%findReceiver(a,[(c1,a),(c2,b),(c3,a)],C1,T1).

%empty(C).
% true if C is empty.
empty(empty).
empty([]).

%greedypath(S,L1,P1)
%produce the shortest path that the robot need to take for a list of pairs of places.
greedypath(empty,[],[]).
greedypath(S,[],[S]).

greedypath(S,L1,P1):-
customer(S),
seperateR(L1,L2),
nearest(S,L2,P2,C),
findReceiver(P2,L1,C1,L4),
greedypath(C1,L4,P3),
append([S,P2],P3,P1).

greedypath(S,L1,P1):-
restaurant(S),
findReceiver(S,L1,C1,L4),
empty(C1),
seperateR(L1,L2),
nearest(S,L2,P2,C),
greedypath(P2,L1,P3),
append([S],P3,P1).

greedypath(S,L1,P1):-
restaurant(S),
findReceiver(S,L1,C1,L4),
\+ empty(C1),
greedypath(C1,L4,P2),
append([S],P2,P1).

%try
%greedypath(a,[(c1,a)],L1).
%greedypath(a,[(c2,b)],L1).
%greedypath(a,[(c2,b),(c1,a)],L1).
%greedypath(a,[(c2,a),(c1,b)],L1).


%end(Path,S).
% S is the end of the list.
end([H],H).
end([H|T],H2):-
end(T,H2).
 
%try
%end([a,b,c,d],S).
 
% nobegin(L1,L2).
% L2 is L1 without the first element.
nobegin([],[]).
nobegin([H|L],L).

%try
%nobegin([a,b,c,d],L).

%greedyOderPath(S,L1,L2,P1).
% given an urgent order L1,not urgent Order L2, produce a path that using greedy algorithm.
greedyOrderPath(_,[],[],[]).
greedyOrderPath(empty,_,_,[]).

greedyOrderPath(S,L1,[],Path1):-
greedypath(S,L1,Path1).


greedyOrderPath(S,[],L2,Path2):-
greedypath(S,L2,Path2).
 
greedyOrderPath(S1,L1,L2,Path12):-
greedypath(S1,L1,Path1),
end(Path1,S2),
greedypath(S2,L2,Path2),
nobegin(Path2,Path3),
append(Path1,Path3,Path12).

%try
% greedyOrderPath(a,[(c1,a)],[],Path).
% greedyOrderPath(a,[],[(c1,a)],Path).
% greedyOrderPath(a,[(c1,a)],[(c1,a)],Path).
% greedyOrderPath(a,[(c1,a),(c1,b)],[(c1,a),(c3,a)],Path).


