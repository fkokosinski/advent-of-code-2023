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

process_line([], _Orientation, []).
process_line(["|"|TailLine], "horizontal", [1|TailOut]) :- !, process_line(TailLine, "horizontal", TailOut).
process_line(["-"|TailLine], "vertical", [1|TailOut]) :- !, process_line(TailLine, "vertical", TailOut).
process_line(["7"|["L"|TailLine]], "vertical", [1|TailOut]) :- !, process_line(TailLine, "vertical", TailOut).
process_line(["F"|["J"|TailLine]], "vertical", [1|TailOut]) :- !, process_line(TailLine, "vertical", TailOut).
process_line(["F"|["J"|TailLine]], "horizontal", [1|TailOut]) :- !, process_line(TailLine, "horizontal", TailOut).
process_line(["L"|["7"|TailLine]], "horizontal", [1|TailOut]) :- !, process_line(TailLine, "horizontal", TailOut).
process_line([_|TailLine], Orientation, [0|TailOut]) :- !, process_line(TailLine, Orientation, TailOut).

is_inside([X, Y], Loop, Matrix) :-
	findall(Elem, (
		address_matrix(Matrix, Var, Y, Elem),
		member(Elem, ["|", "L", "J", "7", "F", "S"]),
		Var > X,
		member([Var, Y], Loop)
	), Right),
	findall(Elem, (
		address_matrix(Matrix, Var, Y, Elem),
		member(Elem, ["|", "L", "J", "7", "F", "S"]),
		Var < X,
		member([Var, Y], Loop)
	), Left),
	findall(Elem, (
		address_matrix(Matrix, X, Var, Elem),
		member(Elem, ["-", "L", "J", "7", "F", "S"]),
		Var < Y,
		member([X, Var], Loop)
	), Top),
	findall(Elem, (
		address_matrix(Matrix, X, Var, Elem),
		member(Elem, ["-", "L", "J", "7", "F", "S"]),
		Var > Y,
		member([X, Var], Loop)
	), Bottom),

	Counts = [[Right, "horizontal"], [Left, "horizontal"], [Top, "vertical"], [Bottom, "vertical"]],

	findall(Mod, (
		member([Count, Orientation], Counts),
		process_line(Count, Orientation, Line),
		sum_list(Line, Sum),
		Mod is Sum mod 2
	), Mods), !,
	list_to_set(Mods, [1]).

main :-
	read_file_to_string('./input_02', File, []),
	split_string(File, '\n', '\n', Lines),
	parse_lines(Lines, Matrix),

	% create a graph
	findall([Elem, X, Y], address_matrix(Matrix, X, Y, Elem), Cells),
	parse_cells(Cells),

	% find the starting position
	findall([X, Y], address_matrix(Matrix, X, Y, "S"), [Start]),

	% find all loops
	findall(Path, get_path_to(Start, Start, [Start], Path), [Path|_]), !,

	% find all fields that are not part of the final loop
	findall([X, Y], (address_matrix(Matrix, X, Y, _), \+member([X, Y], Path)), Fields), !,
	findall(Field, (member(Field, Fields), is_inside(Field, Path, Matrix)), Inside),
	length(Inside, Out),

	% visualize output
	%forall(address_matrix(Matrix, X, Y, Elem), (
	%	(
	%		X =:= 0 ->
	%		nl
	%	;
	%		true
	%	),
	%	(
	%		member([X, Y], Path) ->
	%		write("#")
	%	;
	%		member([X, Y], Inside) ->
	%		write("I")
	%	;
	%		write("O")
	%	)
	%)), nl,

	write(Out), nl,
        halt.

:- initialization(main).
