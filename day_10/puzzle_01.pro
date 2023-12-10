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
	findall(X, (member(Y, Codes), string_codes(X, [Y])), HeadOut),
	parse_lines(TailLine, TailOut).

parse_cells([]).
parse_cells([[Elem, HeadX, HeadY]|TailCell]) :-
	(
		Elem = "|" ->
		% north
		PrevX is HeadX,
		PrevY is HeadY - 1,
		% south
		NextX is HeadX,
		NextY is HeadY + 1
	;
		Elem = "-" ->
		% west
		PrevX is HeadX - 1,
		PrevY is HeadY,
		% east
		NextX is HeadX + 1,
		NextY is HeadY
	;
		Elem = "L" ->
		% north
		PrevX is HeadX,
		PrevY is HeadY - 1,
		% east
		NextX is HeadX + 1,
		NextY is HeadY
	;
		Elem = "J" ->
		% north
		PrevX is HeadX,
		PrevY is HeadY - 1,
		% west
		NextX is HeadX - 1,
		NextY is HeadY
	;
		Elem = "7" ->
		% south
		PrevX is HeadX,
		PrevY is HeadY + 1,
		% west
		NextX is HeadX - 1,
		NextY is HeadY
	;
		Elem = "F" ->
		% south
		PrevX is HeadX,
		PrevY is HeadY + 1,
		% east
		NextX is HeadX + 1,
		NextY is HeadY
	;
		true
	),
	(
		member(Elem, ["|", "-", "L", "J", "7", "F"]) ->
		assertz(next_to([HeadX, HeadY], [PrevX, PrevY])),
		assertz(next_to([HeadX, HeadY], [NextX, NextY]))
	;
		true
	),
	parse_cells(TailCell).

get_path_to(HeadPos, EndPos, HeadPath, Out) :-
	(
		HeadPos = EndPos ->
		findall([X, Y], (next_to([X, Y], HeadPos), \+member([X, Y], HeadPath)), Neighbors)
	;
		findall([X, Y], (next_to(HeadPos, [X, Y]), \+member([X, Y], HeadPath)), Neighbors)
	),

	(
		Neighbors \= [] ->
		member(TailPos, Neighbors),
		get_path_to(TailPos, EndPos, [TailPos|HeadPath], Out)
	;
		next_to(HeadPos, EndPos) ->
		Out = HeadPath
	;
		false
	).

main :-
	read_file_to_string('./input_01', File, []),
	split_string(File, '\n', '\n', Lines),
	parse_lines(Lines, Matrix),

	% create a graph
	findall([Elem, X, Y], address_matrix(Matrix, X, Y, Elem), Cells),
	parse_cells(Cells),

	% find the starting position
	findall([X, Y], address_matrix(Matrix, X, Y, "S"), [Start]),

	% find all loops
	findall(Path, get_path_to(Start, Start, [Start], Path), Paths),

	% calculate max lengt
	findall(Len, (member(Path, Paths), length(Path, Len)), Lengths),
	max_list(Lengths, MaxPath),
	Out is ceil(MaxPath / 2),

	write(Out), nl,
        halt.

:- initialization(main).
