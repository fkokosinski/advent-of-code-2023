#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

valid_red(Count) :- Count =< 12.
valid_green(Count) :- Count =< 13.
valid_blue(Count) :- Count =< 14.

get_color_count(Line, Color, Count) :-
	string_concat('(?<count>\\d+) ', Color, Re),
	\+re_matchsub(Re, Line, _),
	Count = 0.

get_color_count(Line, Color, Count) :-
	string_concat('(?<count>\\d+) ', Color, Re),
	re_matchsub(Re, Line, _{0:_, count:CountStr}),
	atom_number(CountStr, Count).

process_draw([]).
process_draw([H|T]) :-
	get_color_count(H, 'red', Red),
	valid_red(Red),
	get_color_count(H, 'green', Green),
	valid_green(Green),
	get_color_count(H, 'blue', Blue),
	valid_blue(Blue),
	process_draw(T).

process_line(Line, Sum) :-
	re_matchsub('Game (?<id>\\d+)', Line, _{0:_, id:GameIdCodes}),
	atom_number(GameIdCodes, GameId), 
	split_string(Line, ';', ' ', Draws),
	process_draw(Draws),
	Sum = GameId.

process_file([], 0).

process_file([Head|Tail], TotalSum) :-
	process_line(Head, LineSum),
	process_file(Tail, TailSum),
	TotalSum is LineSum + TailSum.

process_file([Head|Tail], Sum) :-
	\+process_line(Head, Sum),
	process_file(Tail, Sum).

file_to_lines(InputFile, LinesOut) :- 
	read_file_to_string(InputFile, Input, []),
	split_string(Input, '\n', '', LinesOut).

main :-
	file_to_lines('./input_01', Lines),
	process_file(Lines, Sum),
	write(Sum), nl,
	halt.

:- initialization(main).
