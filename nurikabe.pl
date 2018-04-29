/* -1 is ground, -2 is wall */

nurikabe([A|L]) :-
    length([A|L], H),
    length(A, W),
    fd_domain_list([A|L], -1, -2,W,H),
    check_count_connected([A|L], 0, 0, W, H),
    check_2x2_grid(0, 0, [A|L], W, H),
    fd_labelingff([A|L]).

fd_domain_list([],_, _,_,_).
fd_domain_list([A|L], Lv, Hv,W,H) :-
    Nb_values is W*H,
    check_values(A, Lv,Hv,Nb_values),
    fd_domain_list(L, Lv,Hv,W,H).

check_values([],_,_,_).		
check_values([A|L],X,Y,Nb_values):- check_values(L,X,Y,Nb_values),A=X.
check_values([A|L],X,Y,Nb_values):- check_values(L,X,Y,Nb_values),A=Y.
check_values([A|L],X,Y,Nb_values):- check_values(L,X,Y,Nb_values),A>=1,A=<Nb_values.
			
    
/* check if two cells are adjacent */
adjacent([X, Ya], [X, Yb]) :-
    Yb is Ya + 1.
adjacent([X, Ya], [X, Yb]) :-
    Yb is Ya - 1.
adjacent([Xa, Y], [Xb, Y]) :-
    Xb is Xa + 1.
adjacent([Xa, Y], [Xb, Y]) :-
    Xb is Xa - 1.

/* two cells are connected if they are the same kind and they are adjacent */
connected(X1, Y1, X2, Y2, Grid) :-
    get_value(X1, Y1, Grid, V1),
    get_value(X2, Y2, Grid, V2),
    same_kind(V1, V2),
    adjacent([X1, Y1], [X2, Y2]).
connected(X1, Y1, X2, Y2, Grid) :-
    connected(X1, Y1, Xtmp, Ytmp, Grid),
    connected(Xtmp, Ytmp, X2, Y2, Grid).

/* count how many squares are connected */
count_connected(_, _, X, Y, _, W, H, _) :-
    X is W - 1,
    Y is H - 1.
count_connected(X, Y, Xi, Yi, Grid, W, H, N) :-
    next_square(Xi, Yi, Xnext, Ynext, W, H),
    connected(X, Y, Xi, Yi, Grid),
    Nnew is N - 1,
    count_connected(X, Y, Xnext, Ynext, Grid, W, H, Nnew).
count_connected(X, Y, Xi, Yi, Grid, W, H, N) :-
    next_square(Xi, Yi, Xnext, Ynext, W, H),
    count_connected(X, Y, Xnext, Ynext, Grid, W, H, N),
    \+connected(X, Y, Xi, Yi, Grid).

/* check if cells are of the same kind */
/* either both are Walls or both are not Walls */
same_kind(Va, Vb) :-
    Va = -2, Vb = -2.
same_kind(Va, Vb) :-
    Va \= -2, Vb \= -2.

/* check if the values in the squares are respected */
check_count_connected(W, H, _, W, H).
check_count_connected(X, Y, Grid, W, H) :-
    get_value(X, Y, -1),
    next_square(X, Y, Xnext, Ynext, Grid, W, H),
    check_count_connected(Xnext, Ynext, Grid, W, H).
check_count_connected(X, Y, Grid, W, H) :-
    get_value(X, Y, -2),
    next_square(X, Y, Xnext, Ynext, Grid, W, H),
    check_count_connected(Xnext, Ynext, Grid, W, H).
check_count_connected(X, Y, Grid, W, H) :-
    get_value(X, Y, Val),
    Val \= -1, Val \= -2,
    count_connected(X, Y, 0, 0, Grid, W, H, Val),
    next_square(X, Y, Xnext, Ynext, Grid, W, H),
    check_count_connected(Xnext, Ynext, Grid, W, H).

/* check bloc of wall */
check_2x2_grid(W, H, _, W, H).
check_2x2_grid(X, Y, Grid, W, H) :-
    get_value(X, Y, Grid, V),
    same_kind(-2, V),
    check_2x2(X, Y, Grid, W, H),
    next_square(X, Y, Xnext, Ynext, W, H),
    check_2x2_grid(Xnext, Ynext, Grid, W, H).
check_2x2_grid(X, Y, Grid, W, H) :-
    get_value(X, Y, Grid, V),
    check_2x2(X, Y, Grid, W, H),
    next_square(X, Y, Xnext, Ynext, W, H),
    check_2x2_grid(Xnext, Ynext, Grid, W, H),
    \+same_kind(-2, V).

check_2x2(X, Y, Grid, W, H) :-
    down(X, Y, X1, Y1, W, H),
    right(X, Y, X2, Y2, W, H),
    down(X2, Y2, X3, Y3, W, H),
    get_value(X1, Y1, Grid, V1),
    get_value(X2, Y2, Grid, V2),
    get_value(X3, Y3, Grid, V3),
    \+three_walls(V1, V2, V3).

/* check if 3 square are wall */
three_walls(V1, V2, V3) :- same_kind(V1, V2), same_kind(V2, V3), same_kind(-2, V1).

/* get the position of the square at the right and below */
down(X, Yini, X, Ynext, _, H) :-
    Ynext is Yini + 1,
    Ynext \= H.
right(Xini, Y, Xnext, Y, W, _) :-
    Xnext is Xini + 1,
    Xnext \= W.

/* get the value of a square */
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
