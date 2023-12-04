#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

calculate_score(0, 0).
calculate_score(Len, Score) :- Score is 2^(Len - 1).

% lines -> [winning numbers, drawn numbers]
process_lines([_], 0).
process_lines([HeadLine|TailLine], HeadSum) :-
	process_lines(TailLine, TailSum),
	split_string(HeadLine, ':', ' ', [_, Numbers]),
	split_string(Numbers, '|', ' ', [WinningString, DrawnString]),
	split_string(WinningString, ' ', ' ', Winning),
	split_string(DrawnString, ' ', ' ', Drawn),
	findall(X, (member(X, Winning), member(X, Drawn)), Bag),
	length(Bag, BagLen),
	calculate_score(BagLen, Score),
	HeadSum is TailSum + Score.

main :-
	read_file_to_string('./input_01', File, []),
	split_string(File, '\n', ' ', Lines),
	process_lines(Lines, Sum),

	write(Sum), nl,
	halt.

:- initialization(main).
