/* -1 is room, -2 is wall */

nurikabe([A|L]) :-
     fd_domain_list([A|L], [-1, -2]),      % step 1
        length([A|L], H),
        length(A, W),
     check_count_connected([A|L], 0, 0, W, H),
     fd_labelingff([A|L]).

fd_domain_list([], _).
fd_domain_list([A|L], Lv) :-
        fd_domain(A, Lv),
        fd_domain_list(L, Lv).
    
/* check if two cells are adjacent */
adjacent([X, Ya], [X, Yb]) :-
    Tmp = Yb,
    Tmp is Ya + 1.
adjacent([X, Ya], [X, Yb]) :-
    Tmp = Yb,
    Tmp is Ya - 1.
adjacent([Xa, Y], [Xb, Y]) :-
    Tmp = Xb,
    Tmp is Xa + 1.
adjacent([Xa, Y], [Xb, Y]) :-
    Tmp = Xb,
    Tmp is Xa - 1.

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
	Va = -1, Vb = -1.
same_kind(Va, Vb) :-
	Va \= -1, Vb \= -1.

	
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
    Xtmp is Xini + 1,
    Xtmp \= W,
    Xnext = Xtmp.
next_square(Xini, Yini, 0, Ynext, W, H) :-
    Xtmp is W - 1,
    Xini = Xtmp,
    Ytmp is Yini + 1,
    Ytmp \= H,
    Ynext = Ytmp.

nurikabe([
    [_, 1, _, _],
    [_, _, _, 2],
    [1, _, 2, _],
    [_, _, _, _],
    [_, _, _, _],
    [2, _, 2, _]
]).
