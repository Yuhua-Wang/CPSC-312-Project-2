% currentShortestPath([To|ReversedPath], C)	
% To is the goal node, ReversedPath is a reversed path. C is cost
% currentShortestPath will store current shortest path and cost from start point to end point
:- dynamic a/0,b/0,c1/0,c2/0,c3/0,currentShortestPath/2.


% connected(X,Y,C)
% is true if the robot can move from location X to location Y with a cost of C
% the following is defined for Map I on google doc
% e.g. robots can go from ubc to kb with a cost of 6:

connected(ubc,kb,6).
connected(kb,ubc,6).
connected(kb,dt,1).
connected(dt,kb,1).
connected(dt,sp,3).
connected(sp,dt,3).
connected(sp,wv,3).
connected(wv,sp,3).
connected(sp,nv,5).
connected(nv,sp,5).
connected(wv,nv,5).
connected(nv,wv,5).
connected(dt,bb,6).
connected(bb,dt,6).
connected(nv,bb,8).
connected(bb,nv,8).
connected(dt,sc,3).
connected(sc,dt,3).
connected(sc,bb,4).
connected(bb,sc,4).
connected(ubc,md,8).
connected(md,ubc,8).
connected(md,sc,4).
connected(sc,md,4).
connected(md,yvr,2).
connected(yvr,md,2).
connected(md,bp,4).
connected(bp,md,4).
connected(bp,yvr,1).
connected(yvr,bp,1).
connected(bp,rb,2).
connected(rb,bp,2).
connected(bp,cr,2).
connected(cr,bp,2).
connected(cr,bb,8).
connected(bb,cr,8).
connected(yvr,rb,1).
connected(rb,yvr,1).

% for mapI
connected(a, c1, 3).
connected(c1, a, 3).
connected(c1, b, 2).
connected(b, c1, 2).
connected(b, c3, 1).
connected(c3, b, 1).
connected(c1, c2, 2).
connected(c2, c1, 2).

% hasFood(C,F) is true if Node C has food F.

hasFood(ubc, fish).
hasFood(ubc, pizza).
hasFood(kb, fish).
hasFood(kb, cheeseBurger).
hasFood(dt, fish).
hasFood(dt, steak).
hasFood(sp, fish).
hasFood(sp, sushi).
hasFood(wv, fish).
hasFood(wv, friedRice).
hasFood(nv, fish).
hasFood(nv, rocketBurger).
hasFood(bb, fish).
hasFood(bb, premiumBeef).
hasFood(sc, fish).
hasFood(sc, hotPot).
hasFood(yvr, fish).
hasFood(yvr, starbucks).
hasFood(md, fish).
hasFood(md, chickenTeriyaki).
hasFood(rb, fish).
hasFood(rb, superFish).
hasFood(bp, fish).
hasFood(bp, fries).
hasFood(cr, fish).
hasFood(cr, surprise).

% hasFood(C,F) is true if Node C has food F. For Map I
hasFood(a, fish).
hasFood(b, fish).
hasFood(c1, fish).
hasFood(c2, fish).
hasFood(c3, fish).

%restaurant(P).
% true if P is a restaurant.
restaurant(a).
restaurant(b).

%customer(P).
% true if P is a customer.
customer(c1).
customer(c2).
customer(c3).



% order(C,Q,R,U,F)
% is true if a customer at location C has ordered Q units of food from restaurants at R, and the urgency level is U. F is food

% the following is defined for Map I
% e.g. customer at ubc has ordered 2 units of fish from kb. This order is urgent.
order(ubc, 2, kb, urgent, fish).
order(ubc, 1, cr, not_urgent, fish).
order(bp, 1, yvr, not_urgent, fish).


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

% edge(From,To,Cost) is true if from From is connected to To directly with distance Cost
edge(From,To,Cost) :- connected(To,From,Cost),
                        connected(From, To, Cost).

% shorterPath2 use Dijkstra algorithm to search for shortest path and cost from start point to destination
% if new Cost is small than old C, then use Cost to replace C.
shorterPath2([H|Path], Cost) :-		      
	currentShortestPath([H|_], C),!, Cost < C,
	retract(currentShortestPath([H|_],_)),
	assert(currentShortestPath([H|Path], Cost)).

% if path does not exist, create a new path.
shorterPath2(Path, Cost) :-		    
	assert(currentShortestPath(Path,Cost)).

% goThroughAllNodes(From, Path, Cost) will go through every node and all unvisited neighbours and update the shortest path and cost 
goThroughAllNodes(From, Path, Cost) :-		   
    edge(From, T, C),
	\+memberchk(T, Path),	  
	S is C+Cost,
	shorterPath2([T,From|Path], S),
	goThroughAllNodes(T,[From|Path], S).	  

% goThroughAllNodes(From) will remove current path and make a new path starting from the start point.
goThroughAllNodes(From) :-
	retractall(currentShortestPath(_,_)),        
	goThroughAllNodes(From,[],0). 

goThroughAllNodes(_). 	

% getShortestPath(From, To, Cost, Path) will return the shortest Path and minimum cost from From to To.
getShortestPath(From, To, Cost, Path) :-
	goThroughAllNodes(From),                  
	currentShortestPath([To|ReversedPath], Cost),        
	reverse([To|ReversedPath], Path).   	


% findpdpair(Order, PDPair).
% is true if PDPair pdpair(P,D) represents a pair of pickup(P) and delivery(D) locations of an order pickup and delivery locations
findpdpair(order(D,_,P,_,_), pdpair(P,D)).


% findAllPdpairs(Orders, PDPairs).
% is true if PDpairs is a list that includes all pdpairs in Orders (a list of orders)
findAllPdpairs([], []).
findAllPdpairs( [order(C,_,R,_,_)|O] ,[pdpair(R,C)|P]) :- findAllPdpairs(O,P).


% findAllLocations(Orders, Locations).
% true if Locations is a list that contains all locations required to fulfill all orders
findAllLocations([],[]).
findAllLocations([order(C,_,P,_,_)|O], [C,P|L]) :- findAllLocations(O,L).


% route(PDPairs, Route, From, AllLocations, Visited, Cost).
% is true if Route is a list of locations which represents a valid route to delivery all pdpairs in PDPairs
% From is the starting location
% AllLocations is a list of all locations required to fulfill all orders.
% Visited is the max number of nodes allowed to be visited (cut the route if to many locations have been visited)
% Cost is the cost of the route
% for each pdpair(R,C), R must be reached before C to complete the order

route([],[],_,_,_,0).
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

% try: findRoute([order(a,2,b,urgent,fish),order(c2,2,c3,urgent,fish)], b, R, C).

% isValidFood(Orders) will check whether in all orders, each restaurant has the food ordered by the customer.
isValidFood([]).
isValidFood([order(_,_,R,_,F)|H]) :-
	hasFood(R,F)->
	isValidFood(H);
	writef('sorry, restaurant at %w does not have food %w\n',[R,F]),
	% hasFood(test,test) is for returning false 
	hasFood(test,test).

% shortestRoute (Orders,Start,Route,Cost).
% is true if Route a list of locations representing the shortest route to fulfill all orders in Orders. Cost is the Cost of the route.
% Start is the starting location.
shortestRoute(O,S,R,C):-
             isValidFood(O),findRoute(O,S,R,C),\+notShortestRoute(O,S,R,C).


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


% append(List1,List2,AppendList).
% is true if L3 is the list made by appending L1 and L2
append([],[],[]).
append(L1,[],L1).
append([],L2,L2).
append([H1|T],L2,[H1|R]):- append(T,L2,R).

%try
% append([a,b],[c],L).
% append([a,b],[],L).
% append([],[c],L).

% sorturgent(Orders,SortedOrders):
% true if L1 is a list of order and L2 conclude all element in L1 in correct sequence(emerge->not emerge).
sorturgent([],[]).
sorturgent([order(P1,X,P2,urgent,_)|T1],[order(P1,X,P2,urgent)|T2]):- sorturgent(T1,T2).
sorturgent([order(P1,X,P2,not_urgent,F)|T1],R2):-
                     append(T2,[order(P1,X,P2,not_urgent,F)],R2), sorturgent(T1,T2).


% norepeat(Orders, NorepeatOrders).
% true if L2 is the same as L1 without duplicates
norepeat([], []).
norepeat([H|T1],L2) :- member(H,T1), norepeat(T1,L2).
norepeat([H|T1],[H|T2]):- \+member(H,T1), norepeat(T1,T2).

%reachable(Orders,ReachableOrders).
% true if L2 contains all elements in L1 whose receiver place is reachable from the restefrant.

reachable([],[]).
reachable([order(P1,X,P2,urgent,F)|T1],[order(P1,X,P2,urgent,F)|T2]):-
                  findpath(P1,P2,_,_),reachable(T1,T2).

reachable([order(P1,X,P2,not_urgent,F)|T1],[order(P1,X,P2,not_urgent,F)|T2]):-
             findpath(P1,P2,_,_),reachable(T1,T2).


%check(Orders,CheckedOders).
% true if L2 contains all legal order in L1.
check([],[]).
check(L1,L4):- norepeat(L1,L2), reachable(L2,L3), sorturgent(L3,L4).

%try
%check([order(ubc,4,yvr,urgent,fish),order(yvr,2,rb,urgent,fish)],L2).
%check([order(ubc,4,yvr,not_urgent,fish),order(yvr,2,rb,not_urgent,fish),order(rb,4,yvr,urgent,fish),order(ubc,4,yvr,urgent,fish)],L2).

% plan(Orders,Cost,Path).
% true if P1 gives the path that the robot can finish all the order.
plan([],0,[]).
plan([order(P1,_,P2,_,_)|L2],C,P30):-
findpath(P1,P2,C1,P10),
plan(L2,C2,P20),
C is C1+C2,
append(P10,P20,P30).

% do(Order,Cost,Path)
% a general Path of Order and its cost are produced.
do([],_,[]).
do(L1,C,P1):-
check(L1,L2),
plan(L2,C,P1).

%try
%do([order(ubc,4,yvr,urgent,fish)],C,P1).
%do([order(ubc,4,yvr,urgent,fish),order(ubc,2,c2,rb,fish)],C,P1).
%do([order(ubc,4,rb,not_urgent,fish),order(cr,2,ubc,not_urgent,fish),order(dt,4,ubc,urgent,fish),order(ubc,4,dt,urgent,fish)],C,P1).
%do([order(rb,4,ubc,urgent,fish)],C,P1).


% seperateByUrgent(Orders,Urgent,Not_Urgent).
% true if the L2 contains all urgent orders in L1 and L3 contains all not_urgent orders in L1 (all orders in L2,L3 as pairs don't have urgency and food).
seperateByUrgent([],[],[]).
seperateByUrgent([order(P1,_,P2,urgent,fish)|T1],[(P1,P2)|T2],L3):- seperateByUrgent(T1,T2,L3).
seperateByUrgent([order(P1,_,P2,not_urgent,fish)|T1],L2,[(P1,P2)|T3]):- seperateByUrgent(T1,L2,T3).

%try
% seperateByUrgent([order(ubc,4,yvr,not_urgent,fish),order(a,4,c1,urgent,fish),order(a,4,c2,urgent,fish)],L2,L3).
% seperateByUrgent([order(ubc,4,c1,yvr,fish),order(rb,4,ubc,urgent,fish)],L2,L3).

% seperateByUrgent([order(ubc, 4, rb, urgent,fish), order(cr, 4, ubc, urgent,fish), order(ubc, 2, rb, not_urgent,fish), order(rb, 4, ubc, not_urgent,fish)] ,L2,L3).

% nearest(Place1,List1,Place2,List2,Cost).
% find the Place1's nearest place in L1,cost C produce as P2 and rest of L1 is L2.
nearest(_,[],empty,1000000).
nearest(empty,[],empty,1000000).

nearest(F1,[P2|T],P3,C1):-
       \+ dif(F1,P2),nearest(F1,T,P3,C1).

nearest(F1,[P2|T],P2,C):-
         dif(F1,P2),shortestPath(F1,P2,C,_),
         nearest(F1,T,_,C1),C<C1.

nearest(F1,[P2|T],P2,C):-
     dif(F1,P2), shortestPath(F1,P2,C,_),
     nearest(F1,T,_,C1), C=C1.

nearest(F1,[P2|T1],P3,C1):-
         dif(F1,P2), shortestPath(F1,P2,C,_),
         nearest(F1,T1,P3,C1),C>C1.

%try
%nearest(a,[a,b,b],P1,C).
%nearest(a,[b,a,c1],P1,C).
%nearest(a,[b,b,b,b,b],P1,C).
%nearest(a,[a],P1,C).

%seperateR(List1,List2).
% find all restaurant in L1.
seperateR([],[]).
seperateR([(_,P2)|T1],[P2|T2]):-
seperateR(T1,T2).

%try
%seperateR([(a,c1)],L2).
%seperateR([(c1,a),(c2,b)],L2).
%seperateR([(c2, b),  (c1, b)],L).
%seperateR([(c3, a),  (c1, a)],L).

%findReceiver(Restaurant1,List1,Reiceiver1,List2).
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

%greedypath(Start,Order1,Path1)
%produce the shortest path that the robot need to take for a list of pairs of places.
greedypath(empty,[],[]).
greedypath(S,[],[S]).

greedypath(S,L1,P1):-
         customer(S),seperateR(L1,L2),nearest(S,L2,P2,_),
         findReceiver(P2,L1,C1,L4),greedypath(C1,L4,P3),append([S,P2],P3,P1).

greedypath(S,L1,P1):-
         restaurant(S), findReceiver(S,L1,C1,_), empty(C1),
         seperateR(L1,L2), nearest(S,L2,P2,_),
         greedypath(P2,L1,P3), append([S],P3,P1).

greedypath(S,L1,P1):-
         restaurant(S), findReceiver(S,L1,C1,L4),
         \+ empty(C1),greedypath(C1,L4,P2),append([S],P2,P1).

%try
%greedypath(a,[(c1,a)],L1).
%greedypath(a,[(c2,b)],L1).
%greedypath(a,[(c2,b),(c1,a)],L1).
%greedypath(a,[(c2,a),(c1,b)],L1).


%end(Path,End).
% S is the end of the list.
end([H],H).
end([_|T],H2):- end(T,H2).
 
%try
%end([a,b,c,d],S).
 
% nobegin(List1,List2).
% L2 is L1 without the first element.
nobegin([],[]).
nobegin([_|L],L).

%try
%nobegin([a,b,c,d],L).

%greedyOderPath(Start,List1,List2,Path1).
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

go(Orders,Start) :- 
	 shortestRoute(Orders,Start,Route,Cost)->
	 writef('shortest route is %w with cost %w\n', [Route, Cost]);
	 writef('sorry, the path does not exist'),
	 % should return false.
	 hasFood(test,test).

%try
% go([order(ubc,2,rb,urgent,fish),order(ubc,2,bp,urgent,fish)], rb).
% go([order(ubc,2,rb,urgent,superFish),order(ubc,2,bp,urgent,fries)], rb).
% go([order(cr,2,rb,urgent,hotPot),order(cr,2,bp,urgent,fries)], rb).
% go([order(bp,3,cr,urgent,surprise), order(md,5,sc,urgent,hotPot),order(bp,1,md,urgent,fish)], bb).
% go([order(bp,3,cr,urgent,fish), order(md,5,sc,urgent,fish),order(ubc,1,yvr,urgent,fish)], bb).
% go([order(bp,3,cr,urgent,surprise), order(md,5,sc,urgent,hotPot),order(ubc,1,yvr,urgent,fish)], bb).




