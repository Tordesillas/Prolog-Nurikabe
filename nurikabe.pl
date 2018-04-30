/* -1 is ground, -2 is wall */

nurikabe([A|L]) :-
    length([A|L], H),
    length(A, W),
    fd_domain_list([A|L], -1, -2, W, H),
    check_count_grid(0, 0, [A|L], W, H),
    check_2x2_grid(0, 0, [A|L], W, H),
    check_walls([A|L], W, H),
    flatten([A|L], S),
    fd_labelingff(S).

/* set the domain of the grid */
fd_domain_list([],_, _,_,_).
fd_domain_list([A|L], Lv, Hv,W,H) :-
    Nb_values is W*H,
    check_values(A, Lv,Hv,Nb_values),
    fd_domain_list(L, Lv,Hv,W,H).		

check_values([],_,_,_).		
check_values([A|L],X,Y,Nb_values) :- check_values(L,X,Y,Nb_values), A=X.
check_values([A|L],X,Y,Nb_values) :- check_values(L,X,Y,Nb_values), A=Y.
check_values([A|L],X,Y,Nb_values) :- check_values(L,X,Y,Nb_values), A>=1, A=<Nb_values.

/* check if two squares are connected */
connected(X1, Y1, X2, Y2, Grid, L) :-
    get_value(X1, Y1, Grid, V1),
    get_value(X2, Y2, Grid, V2),
    same_kind(V1, V2),
    \+ memberchk([X2, Y2], L).

/* find the square adjacent */
d(X, Y, X, Y, _, H) :- Y is H - 1.
d(X, Ya, X, Yb, _, H) :- Yb is Ya + 1, Ya \= H - 1.
u(X, 0, X, 0, _, _).
u(X, Ya, X, Yb, _, _) :- Yb is Ya - 1, Ya \= 0.
r(X, Y, X, Y, W, _) :- X is W - 1.
r(Xa, Y, Xb, Y, W, _) :- Xb is Xa + 1, Xa \= W - 1.
l(0, Y, 0, Y, _, _).
l(Xa, Y, Xb, Y, _, _) :- Xb is Xa - 1, Xa \= 0.

/* check number of squares in an island */
check_count_island(X, Y, Grid, W, H) :-
    connect_adjacents(X, Y, [[X,Y]], L, Grid, W, H),
    get_value(X, Y, Grid, N),
    length(L, N).

add_if_connected(X, Y, Xi, Yi, L, L, Grid, _, _) :-
    \+connected(X, Y, Xi, Yi, Grid, L).
add_if_connected(X, Y, Xi, Yi, L, Lnew, Grid, W, H) :-
    connected(X, Y, Xi, Yi, Grid, L),
    connect_adjacents(Xi, Yi, [[Xi,Yi] | L], Lnew, Grid, W, H).

connect_adjacents(X, Y, L, Lnew, Grid, W, H) :-
    d(X, Y, X1, Y1, W, H),
    u(X, Y, X2, Y2, W, H),
    r(X, Y, X3, Y3, W, H),
    l(X, Y, X4, Y4, W, H),
    add_if_connected(X, Y, X1, Y1, L, L2, Grid, W, H),
    add_if_connected(X, Y, X2, Y2, L2, L3, Grid, W, H),
    add_if_connected(X, Y, X3, Y3, L3, L4, Grid, W, H),
    add_if_connected(X, Y, X4, Y4, L4, Lnew, Grid, W, H).

/* check all the positive numbers in the grid */
check_count_grid(W, H, _, W, H).
check_count_grid(X, Y, Grid, W, H) :-
    get_value(X, Y, Grid, Val),
    next_square(X, Y, Xnext, Ynext, W, H),
    check_count_grid(Xnext, Ynext, Grid, W, H),
    Val < 0.
check_count_grid(X, Y, Grid, W, H) :-
    get_value(X, Y, Grid, Val),
    next_square(X, Y, Xnext, Ynext, W, H),
    check_count_grid(Xnext, Ynext, Grid, W, H),
    Val > 0,
    check_count_island(X, Y, Grid, W, H).

/* check if cells are of the same kind */
/* either both are Walls or both are not Walls */
same_kind(Va, Vb) :-
    Va = -2, Vb = -2.
same_kind(Va, Vb) :-
    Va \= -2, Vb \= -2.

/* check blocs of wall in the grid */
check_2x2_grid(_, Y, _, _, H) :- Y is H - 1.
check_2x2_grid(X, Y, Grid, W, H) :-
    get_value(X, Y, Grid, V),
    same_kind(-2, V),
    check_2x2(X, Y, Grid, W, H),
    next_square(X, Y, Xnext, Ynext, W, H),
    check_2x2_grid(Xnext, Ynext, Grid, W, H).
check_2x2_grid(X, Y, Grid, W, H) :-
    get_value(X, Y, Grid, V),
    next_square(X, Y, Xnext, Ynext, W, H),
    check_2x2_grid(Xnext, Ynext, Grid, W, H),
    \+same_kind(-2, V).

/* check a bloc of 2x2 squares */
check_2x2(X, _, _, W, _) :- X is W - 1.
check_2x2(X, Y, Grid, W, H) :-
    down(X, Y, X1, Y1, W, H),
    right(X, Y, X2, Y2, W, H),
    down(X2, Y2, X3, Y3, W, H),
    get_value(X1, Y1, Grid, V1),
    get_value(X2, Y2, Grid, V2),
    get_value(X3, Y3, Grid, V3),
    \+three_walls(V1, V2, V3).

/* check if 3 squares are walls */
three_walls(V1, V2, V3) :- same_kind(V1, V2), same_kind(V2, V3), same_kind(-2, V1).

/* get the position of the square at the right and below */
down(X, Yini, X, Ynext, _, H) :-
    Ynext is Yini + 1,
    Ynext \= H.
right(Xini, Y, Xnext, Y, W, _) :-
    Xnext is Xini + 1,
    Xnext \= W.

/* find the value of a square in the grid */
/* L is a grid with the value of each square */
get_value(X, Y, [_|L], Val) :-
    Ytmp is Y - 1,
    get_value(X, Ytmp, L, Val).
get_value(X, 0, [A|_], Val) :-
    get_value_line(X, A, Val).

get_value_line(X, [_|L], Val) :-
    Xtmp is X - 1,
    get_value_line(Xtmp, L, Val).
get_value_line(0, [Val|_], Val). 

/* find the next square */
/* the function stop at the position (W,H) */
next_square(X, Y, W, H, W, H) :-
    X is W - 1,
    Y is H - 1.
next_square(Xini, Y, Xnext, Y, W, _) :-
    Xnext is Xini + 1,
    Xnext \= W.
next_square(Xini, Yini, 0, Ynext, W, H) :-
    Xini is W - 1,
    Ynext is Yini + 1,
    Ynext \= H.

/* count the number of walls in the grid */
count_walls(W, H, _, W, H, 0).
count_walls(X, Y, Grid, W, H, N) :-
    get_value(X, Y, Grid, -2),
    next_square(X, Y, Xnext, Ynext, W, H),
    count_walls(Xnext, Ynext, Grid, W, H, Nnew),
    N is Nnew + 1,
    X \= W.
count_walls(X, Y, Grid, W, H, N) :-
    get_value(X, Y, Grid, Val),
    next_square(X, Y, Xnext, Ynext, W, H),
    count_walls(Xnext, Ynext, Grid, W, H, N),
    Val \= -2,
    X \= W.

/* count walls v2 */
count_wall([],0).
count_wall([A|L],N) :-
    count_wall_line(A,N1),
    count_wall(L,N2),
    N is N1+N2.
	
count_wall_line([],0).
count_wall_line([-2|T],Y) :-
    count_wall_line(T,Z),
    Y is 1+Z.
count_wall_line([_|T],Z) :-
    count_wall_line(T,Z).

/* check there is an unique wall */
check_walls(Grid, W, H) :- \+find_wall(0, 0, _, _, Grid, W, H).
check_walls(Grid, W, H) :-
    find_wall(0, 0, X, Y, Grid, W, H),
    check_walls(X, Y, Grid, W, H).

check_walls(X, Y, Grid, W, H) :-
    connect_adjacents(X, Y, [[X,Y]], L, Grid, W, H),
    count_walls(0, 0, Grid, W, H, N),
    length(L, N).

find_wall(X, Y, X, Y, Grid, _, _) :- get_value(X, Y, Grid, -2).
find_wall(Xi, Yi, X, Y, Grid, W, H) :-
    get_value(Xi, Yi, Grid, Val),
    next_square(Xi, Yi, Xnext, Ynext, W, H),
    find_wall(Xnext, Ynext, X, Y, Grid, W, H),
    Val \= -2.

/*
nurikabe([
    [_, 1, _, _],
    [_, _, _, 2],
    [1, _, 2, _],
    [_, _, _, _],
    [_, _, _, _],
    [2, _, 2, _]
]).

nurikabe([
    [_, _, _, _, _, _],
    [_, _, _, _, _, 5],
    [_, 2, _, _, 3, _],
    [_, _, _, _, _, _],
    [2, _, _, _, _, _],
    [_, _, 5, _, _, _]
]).

nurikabe([
    [_, 4, _, 5, _],
    [_, _, _, _, _],
    [_, _, 1, _, _],
    [4, _, _, _, _],
    [_, _, _, _, _]
]).

nurikabe([
    [_, _, _, 2, _, _,_,_,_,_],
    [_, _, _, _, _, 2,_,_,_,_],
    [_, _, 7, _, _, _,_,1,_,_],
    [_, _, _, _, 2, _,_,_,_,5],
    [_, 2, _, _, _, _,2,_,_,_],
    [_, _, _, 2, _, _,_,_,2,_],
    [7, _, _, _, _, 2,_,_,_,_],
    [_, _, 2, _, _, _,_,3,_,_],
    [_, _, _, _, 1, _,_,_,_,_],
    [_, _, _, _, _, _,3,_,_,_]
]).*/
