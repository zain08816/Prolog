% CS 314 Fall 2020, HW9
% Due: December 10, 10:00 PM (ET)
%
% Submissions received up to six hours past the deadline will be accepted with reduced
% scores.
%
% For this assignment, submit a file named hw9.pl or hw9.pro containing the definitions
% given described below. You may use this file directly, or create your own.
%
% Each relation is worth 10 points. You will receive 9 points if your code works correctly
% in the specified mode, and an additional 1 point if your code produces answers for the
% "most general query". That is, if the relation produces infinitely many answers when
% all parameters are uninstantiated.
%
% You will need to define additional relations to support your definitions.

% import CLP(FD) definitions -- strongly encouraged
:- use_module(library(clpfd)).

% zip(+List1, +List2, -List3) is deterministic
%
% zip/3 is a relation between three lists of the same length, where each element of
% List3 is a pair X-Y, where X and Y are the corresponding elements of List1 and List2.
%
% e.g., zip([a,b,c],[d,e,f],[a-d, b-e, c-f]).
%
% Note: while zip/3 is only true when all three lists are the same length, you should not
% need to check this explicitly: it should be a consequence of your definition.
%
% zip/3 can be defined in two rules, using no other relations.
%
% If you have defined zip/3 well, it will produce infinitely many answers if all three
% arguments are uninstantiated.

zip([], [], []).
zip([X|Xs],[Y|Ys],[X-Y|Xys]) :- zip(Xs, Ys, Xys).

% sorted(++List) is deterministic
%
% sorted/1 holds if its argument is a list of integers in nondecreasing order.
%
% sorted([]).
% sorted([1,2,5,18]).
% sorted([2,18,18,18,2000]).
%
% Note: Because you are using CLP(FD), sorted/1 should produce a single answer even if the
% list contains uninstantiated variables, and should produce infinitely many lists if
% the list is partially uninstantiated.

sorted([_]).
sorted([]).
sorted([X,Y|Xs]) :- X #=< Y , sorted([Y|Xs]).
sorted([X,Y|[]]) :- X #=< Y.

% symmetric(+Tree) is deterministic
%
% symmetric/1 holds if its argument is a symmetric binary tree.
%
% symmetric(tip)
% symmetric(bin(bin(tip,1,bin(tip,2,tip)),3,bin(bin(tip,2,tip),1,tip)))
%
% symmetric/1 must work properly if the tree contains variables.
%
% ?- symmetric(bin(bin(tip,A,bin(tip,B,tip)),C,bin(bin(tip,D,tip),E,tip))).
% A = E,
% B = D.
%
% Note: symmetric/1 should produce infinitely many symmetric trees if its argument is
% uninstantiated, but does not need to produce every such tree.

mirror(tip,tip).
mirror(bin(OutsideL,X,InsideR), bin(InsideR,X,OutsideR)) :- mirror(OutsideL,OutsideR), mirror(InsideR,InsideR).

symmetric(tip).
symmetric(bin(L,_,R)) :- mirror(L, R).

% insert_tree(+X, ++InitialBST, -FinalBST) is deterministic
%
% insert_tree/3 is a relation between an integer and two binary search trees containing
% integers. It holds when FinalBST is the result of inserting X into InitialBST. (If X
% is present in InitialBST, then FinalBST = InitialBST.)
%
% insert_tree/3 may assume that InitialBST is a valid binary search tree.
%
% If you have defined insert_tree/3 well, it will work in reverse. E.g.,
%
% ?- insert_tree(3, T, bin(tip,3,tip)).
% T = tip ;
% T = bin(tip, 3, tip) ;
% false.

insert_tree(X, bin(L,A,R), bin(LX,A,R)) :- A #> X, insert_tree(X,L,LX).
insert_tree(X, bin(L,A,R), bin(L,A,RX)) :- A #< X, insert_tree(X,R,RX).
insert_tree(X, bin(L,A,R), bin(L,A,R)) :- A #= X.
insert_tree(X, tip, bin(tip, X, tip)).
insert_tree(X, bin(tip, X, tip), tip).
% insert_tree(X, _, bin(tip, X, tip)).

% %%%%%%%%%%%%%%%%%%%%%%%%
% Extra Credit (10 points)
% %%%%%%%%%%%%%%%%%%%%%%%%

% route(+Source, +Destination, -Route)
%
% route/3 is a relation that holds between two locations and a route between them. A route
% is a list of triples of the form Src-Type-Dst, where connect(Src, Dst, Type) holds,
% where the destination of each step is the source of the next, and no location is visited
% more than once.
%
% ?- route(manhattan, bronx, R).
% R = [manhattan-bridge-bronx] ;
% R = [manhattan-bridge-brooklyn, brooklyn-road-queens, queens-bridge-bronx] ;
% R = [manhattan-ferry-staten_island, staten_island-bridge-brooklyn, brooklyn-road-queens, queens-bridge-bronx] ;
% R = [manhattan-bridge-queens, queens-bridge-bronx] ;
% false.


% route1(Source, Destination, [Source, Destination]) :- connectS(Source, Destination).
% route1(Source, Destination, [Source-Type-Destination]) :- connect(Source, Destination, Type).
route1(Source,Destination,Path) :-
    route2(Source,Destination,Visited,[Source]), 
    reverse(Visited,Path).

% route_(Source, Destination, [Source, Destination], _) :- connectS(Source, Destination).
route2(Source,Destination,[Destination|Path],Path) :- connectS(Source,Destination).
route2(Source,Destination,Path,Visited) :-
    connectS(Source,Destination_Next),
    dif(Destination_Next,Destination),         
    nonmember(Destination_Next,Visited),
    route2(Destination_Next,Destination,Path,[Destination_Next|Visited]).  
    
% route2(Source, Destination, [Source-Type-Destination],_) :- connect(Source, Destination, Type).  
% route2(Source, Destination, [Source-Type-Destination|Path],Path) :- connect(Source, Destination, Type).
% route2(Source, Destination, [Source-Type-Destination|Path], Visited) :-
%     connect(Source, Destination_Next, Type),
%     nonmember(Destination_Next, Visited),
%     route2(Destination_Next, Destination, Path, [Destination-Type-Destination_Next|Visited]).

join_path([], []).
join_path([Source, Destination|[]], [Source-Type-Destination|[]]) :- connect(Source, Destination, Type).
join_path([Source, Destination|Rest], [Source-Type-Destination|Path]) :- 
    connect(Source, Destination, Type),
    join_path([Destination|Rest], Path).

route(Source, Destination, Route) :-
    route1(Source, Destination, Path),
    join_path(Path, Route).


% route(Source, Destination, [Source-Type-Destination]) :- connect(Source, Destination, Type).
% route(Destination, Destination, [Source-Type-Destination]) :- connect(Source, Destination, Type).
% route(Source, Destination_Final, [Source-Type-Destination, Destination-Type_Next-Destination_Next|Rest]) :- 
%     connect(Source, Destination, Type),
%     connect(Destination, Destination_Next, Type_Next),
%     % hasnt_been(Destination, Rest),
%     % nonmember(Source-Type-Destination, [Destination-Type_Next-Destination_Next|Rest]),
%     nonmember(Destination-Type-Source, [Destination-Type_Next-Destination_Next|Rest]),
%     nonmember(Source-_-_, [Destination-Type_Next-Destination_Next|Rest]),
%     nonmember(_-_-Source, [Destination-Type_Next-Destination_Next|Rest]),
%     route(Destination, Destination_Final, [Destination-Type_Next-Destination_Next|Rest]).


% connect/3 is a relation between two locations and a connection type. It is symmetric,
% in that connect(S,D,T) holds if and only if connect(D,S,T) holds.

connectS(Src, Dst) :- connect_(Src, Dst, _); connect_(Dst, Src, _).

connect(Src, Dst, Type) :- connect_(Src, Dst, Type); connect_(Dst, Src, Type).

% connect_/3 is the underlying relation used by connect/3. Unlike connect/3, it is not
% symmetric.
%
% Feel free to add additional locations and connections!


connect_(manhattan, bronx, bridge).
connect_(manhattan, brooklyn, bridge).
connect_(manhattan, staten_island, ferry).
connect_(manhattan, queens, bridge).
connect_(staten_island, brooklyn, bridge).
connect_(brooklyn, queens, road).
connect_(queens, bronx, bridge).

% nonmember/2 is a relation that holds between an element and a list that does not contain
% the element.

nonmember(_, []).
nonmember(X, [Y|Ys]) :- dif(X,Y), nonmember(X,Ys).
