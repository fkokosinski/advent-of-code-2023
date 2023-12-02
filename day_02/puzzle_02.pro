#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

get_color_count(Line, Color, Count) :-
	string_concat('(?<count>\\d+) ', Color, Re),
	\+re_matchsub(Re, Line, _),
	Count = 0.

get_color_count(Line, Color, Count) :-
	string_concat('(?<count>\\d+) ', Color, Re),
	re_matchsub(Re, Line, _{0:_, count:CountStr}),
	atom_number(CountStr, Count).

process_draw([], [], [], []).
process_draw([H|T], [HRed|TRed], [HGreen|TGreen], [HBlue|TBlue]) :-
	get_color_count(H, 'red', HRed),
	get_color_count(H, 'green', HGreen),
	get_color_count(H, 'blue', HBlue),
	process_draw(T, TRed, TGreen, TBlue).

process_line(Line, Sum) :-
	split_string(Line, ';', ' ', Draws),
	process_draw(Draws, Reds, Greens, Blues),
	max_list(Reds, MaxRed),
	max_list(Greens, MaxGreen),
	max_list(Blues, MaxBlue),
	Sum is MaxRed * MaxGreen * MaxBlue.

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
