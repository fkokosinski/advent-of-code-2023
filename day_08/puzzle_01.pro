#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

parse_lines([]).
parse_lines([HeadLine|TailLine]) :-
	re_matchsub("(?<current>\\w+) = \\((?<left>\\w+), (?<right>\\w+)\\)", HeadLine, Sub),
	Current = Sub.get('current'),
	Left = Sub.get('left'),
	Right = Sub.get('right'),

	string_codes(Current, CurrentCodes),
	string_codes(Left, LeftCodes),
	string_codes(Right, RightCodes),

	assertz(next_position(CurrentCodes, 'left', LeftCodes) :- !),
	assertz(next_position(CurrentCodes, 'right', RightCodes) :- !),

	parse_lines(TailLine).

code_to_move(82, 'right').
code_to_move(76, 'left').

count_steps([90, 90, 90], _, Count, Count).
count_steps(Pos, [], CurCount, OutCount) :-
	get_moves(Moves),
	count_steps(Pos, Moves, CurCount, OutCount).

count_steps(HeadPos, [HeadMove|TailMove], CurCount, OutCount) :-
	code_to_move(HeadMove, NextMove),
	next_position(HeadPos, NextMove, TailPos),
	NextCount is CurCount + 1, !,
	count_steps(TailPos, TailMove, NextCount, OutCount).

main :-
	read_file_to_string('./input_01', File, []),
	split_string(File, '\n', '\n', [Moves|Lines]),
	parse_lines(Lines),

	string_codes(Moves, MovesCodes),
	assertz(get_moves(MovesCodes)),

	string_codes("AAA", StartPoint),
	count_steps(StartPoint, [], 0, Out),

	write(Out), nl,
        halt.

:- 
	set_prolog_flag(stack_limit, 26_442_450_944),
	initialization(main).
