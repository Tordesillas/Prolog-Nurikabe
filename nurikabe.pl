/* -1 is ground, -2 is wall */

nurikabe([A|L]) :-
     fd_domain_list([A|L], [-1, -2]),
     length([A|L], H),
     length(A, W),
     check_count_connected([A|L], 0, 0, W, H),
     check_count_connected([A|L], 0, 0, W, H),
     fd_labelingff([A|L]).

fd_domain_list([], _).
fd_domain_list([A|L], Lv) :-
        fd_domain(A, Lv),
        fd_domain_list(L, Lv).
    
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
/* V: kind ; C: coordinates ; W: width of board : H: height of board */
connected([Va|Ca], [Vb|Cb], W, H) :-
	same_kind(Va, Vb),
	adjacent(Ca, Cb).
/* transitivity of connections */
connected(A, C, W, H) :-
	connected(A, B, W, H),
	connected(B, C, W, H). 

/* count cells connected  with A */
count_connected(_, [], _, _, _).
/* argument B is a list, go deeper */
count_connected(La, [B|L], Xb, Yb, W, H, N) :-
	is_list(B),
	count_connected(La, B, Xb, Yb, W, H, N),
	count_connected(La, L, Xb, Yb, N).

count_connected(La, [B|L], Xb, Yb, W, H, N1) :-
	\+is_list(B),
	increment_if_connected(La, [B, Xb, Yb], W, H, N, N1),
	count_connected(La, L, Xb, Yb, W, H, N).
	
/* increment N in N1 if A is connected to B */
increment_if_connected(La, Lb, W, H, N, N1) :-
	N1 is N + 1,
	connected(La, Lb, W, H).
increment_if_connected(La, Lb, W, H, N, N1) :-
	N1 is N,
	\+connected(La, Lb, W, H).

/* check if cells are of the same kind */
/* either both are Walls or both are not Walls */
same_kind(Va, Vb) :-
	Va = -2, Vb = -2.
same_kind(Va, Vb) :-
	Va \= -2, Vb \= -2.

	
check_count_connected([], _, _, _, _).
/* argument is a list, go deeper */
check_count_connected([A|L], X, Y, W, H) :-
	is_list(A),
	X1 is X+1,
	check_count_connected(A, X1, Y, W, H),
	Y1 is Y+1,
	check_count_connected(L, 0, Y1, W, H).
check_count_connected([A|L], X, Y, W, H) :-
	\+is_list(A),
	A >= 0,
	count_connected([A, X, Y], 0, 0, W, H, N),
	A = N,
	X1 is X+1,
	check_count_connected(L, X1, Y, W, H).
/* not a number, don't count */
check_count_connected([A|L], X, Y, W, H) :-
	\+is_list(A),
	A < 0,
	X1 is X + 1,
	check_count_connected(L, X1, Y, W, H).

/* check bloc of wall */
check_2x2_grid(X, Y, _, W, H) :-
    X is W - 1,
    Y is H - 1. 
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
    \+three_wall(V1, V2, V3).

/* check if 3 square are wall */
three_wall(V1, V2, V3) :- same_kind(V1, V2), same_kind(V2, V3), same_kind(-2, V1).

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
next_square(Xini, Y, Xnext, Y, W, _) :-
    Xnext is Xini + 1,
    Xnext \= W.
next_square(Xini, Yini, 0, Ynext, W, H) :-
    Xini is W - 1,
    Ynext is Yini + 1,
    Ynext \= H.

nurikabe([
    [_, 1, _, _],
    [_, _, _, 2],
    [1, _, 2, _],
    [_, _, _, _],
    [_, _, _, _],
    [2, _, 2, _]
]).
