#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

parse_lines([], []).
parse_lines([HeadLine|TailLine], [HeadOut|TailOut]) :-
	split_string(HeadLine, ' ', ' ', Split),
	findall(X, (member(Y, Split), number_string(X, Y)), HeadOut),
	parse_lines(TailLine, TailOut).

get_differences([Num1, Num2], [Diff]) :- Diff is Num2 - Num1.
get_differences([HeadNum|TailNum], [HeadDiff|TailDiff]) :-
	[Num2|_] = TailNum,
	HeadDiff is Num2 - HeadNum,
	get_differences(TailNum, TailDiff).

process_line(Seq, 0) :- list_to_set(Seq, [0]).
process_line(HeadSeq, HeadDiff) :-
	get_differences(HeadSeq, TailSeq),

	process_line(TailSeq, TailDiff),
	last(HeadSeq, Last),
	HeadDiff is TailDiff + Last.

process_lines([], []).
process_lines([HeadLine|TailLine], [HeadOut|TailOut]) :-
	process_line(HeadLine, HeadOut),
	process_lines(TailLine, TailOut).

main :-
	read_file_to_string('./input_01', File, []),
	split_string(File, '\n', '\n', Lines),
	parse_lines(Lines, Converted),

	process_lines(Converted, Extrapolated),
	sum_list(Extrapolated, Out),

	write(Out), nl,
        halt.

:- initialization(main).
