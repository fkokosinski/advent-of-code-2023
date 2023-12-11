#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

address_matrix(Matrix, X, Y, Out) :-
	nth0(Y, Matrix, Row),
	nth0(X, Row, Out).

parse_lines([], []).
parse_lines([HeadLine|TailLine], [HeadOut|TailOut]) :-
	string_codes(HeadLine, Codes),
	findall(X, (member(Y, Codes), string_codes(X, [Y])), HeadOut), !,
	parse_lines(TailLine, TailOut).

is_row_empty(Matrix, Y) :-
	findall(Elem, address_matrix(Matrix, _, Y, Elem), Row),
	\+member("#", Row).

is_column_empty(Matrix, X) :-
	findall(Elem, address_matrix(Matrix, X, _, Elem), Column),
	\+member("#", Column).

dup_row_elements([], _DupList, _Idx, []).
dup_row_elements([HeadEl|TailEl], DupList, HeadIdx, [HeadEl|[HeadEl|TailOut]]) :-
	member(HeadIdx, DupList),
	TailIdx is HeadIdx + 1,
	dup_row_elements(TailEl, DupList, TailIdx, TailOut).

dup_row_elements([HeadEl|TailEl], DupList, HeadIdx, [HeadEl|TailOut]) :-
	TailIdx is HeadIdx + 1,
	dup_row_elements(TailEl, DupList, TailIdx, TailOut).

process_matrix([], _DupList, []).
process_matrix([HeadRow|TailRow], DupList, [HeadOut|TailOut]) :-
	member("#", HeadRow),
	dup_row_elements(HeadRow, DupList, 0, HeadOut),
	process_matrix(TailRow, DupList, TailOut).

process_matrix([HeadRow|TailRow], DupList, [HeadOut|[HeadOut|TailOut]]) :-
	dup_row_elements(HeadRow, DupList, 0, HeadOut),
	process_matrix(TailRow, DupList, TailOut).

manhattan_dist([X1, Y1], [X2, Y2], Columns, Rows, Dist) :-
	findall(1_999_998, (member(X, Columns), X1 < X, X < X2), ColumnsDup),
	sum_list(ColumnsDup, ColumnsSum),
	findall(1_999_998, (member(Y, Rows), Y1 < Y, Y < Y2), RowsDup),
	sum_list(RowsDup, RowsSum),

	X is ColumnsSum + abs(X1 - X2),
	Y is RowsSum + abs(Y1 - Y2),
	Dist is X + Y.

main :-
	read_file_to_string('./input_02', File, []),
	split_string(File, '\n', '\n', Lines),
	parse_lines(Lines, Matrix),

	findall(X, (address_matrix(Matrix, X, _, _), is_column_empty(Matrix, X)), ColumnsDup),
	list_to_set(ColumnsDup, Columns),
	findall(Y, (address_matrix(Matrix, _, Y, _), is_row_empty(Matrix, Y)), RowsDup),
	list_to_set(RowsDup, Rows),

	findall([X, Y], address_matrix(Matrix, X, Y, "#"), Galaxies),
	findall(Dist, (
		member(Gal1, Galaxies),
		member(Gal2, Galaxies),
		Gal1 \= Gal2,
		manhattan_dist(Gal1, Gal2, Columns, Rows, Dist)
	), Dists),

	sum_list(Dists, OutDup),
	Out is OutDup / 2,
	write(Out), nl,
	halt.

:- initialization(main).
