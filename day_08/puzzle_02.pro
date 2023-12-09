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
	assertz(is_position(CurrentCodes)),

	parse_lines(TailLine).

code_to_move(82, 'right').
code_to_move(76, 'left').

count_steps([_, _, 90], _, Count, Count) :- !.
count_steps(Pos, [], CurCount, OutCount) :-
	get_moves(Moves),
	count_steps(Pos, Moves, CurCount, OutCount).

count_steps(HeadPos, [HeadMove|TailMove], CurCount, OutCount) :-
	code_to_move(HeadMove, NextMove),
	next_position(HeadPos, NextMove, TailPos),
	NextCount is CurCount + 1, !,
	count_steps(TailPos, TailMove, NextCount, OutCount).

find_lcm(HeadLCM, [X], Out) :- Out is lcm(HeadLCM, X).
find_lcm(HeadLCM, [H|T], Out) :-
	TailLCM is lcm(HeadLCM, H),
	find_lcm(TailLCM, T, Out).

main :-
	read_file_to_string('./input_01', File, []),
	split_string(File, '\n', '\n', [Moves|Lines]),
	parse_lines(Lines),

	string_codes(Moves, MovesCodes),
	assertz(get_moves(MovesCodes)),

	findall(X, (is_position(X), X = [_, _, 65]), StartingPositions),
	findall(X, (member(Y, StartingPositions), count_steps(Y, [], 0, X)), Counts),
	find_lcm(1, Counts, Out),

	write(Out), nl,
        halt.

:- initialization(main).
