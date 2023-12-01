#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

first_number([Head|_], Num) :-
	string_codes(Char, [Head]),
	atom_number(Char, Num).

first_number([Head|Str], Num) :-
	string_codes(Char, [Head]),
	\+atom_number(Char, Num),
	first_number(Str, Num).

process_regex(LineIn, LineIn, [], []).

process_regex(LineIn, LineOut, [PatternH|PatternT], [ReplaceH|ReplaceT]) :-
	re_replace(PatternH, ReplaceH, LineIn, Line),
	process_regex(Line, LineOut, PatternT, ReplaceT).

process_line(Line, Sum) :-
	Patterns = ['zero'/g, 'one'/g, 'two'/g, 'three'/g, 'four'/g, 'five'/g, 'six'/g, 'seven'/g, 'eight'/g, 'nine'/g],
	Replaces = ['z0o', 'o1e', 't2o', 't3e', 'f4r', 'f5e', 's6x', 's7n', 'e8t', 'n9e'],
	process_regex(Line, LineOut, Patterns, Replaces),
	string_codes(LineOut, Codes),
	reverse(Codes, RevCodes),
	first_number(Codes, FirstNum),
	first_number(RevCodes, LastNum),
	Sum is FirstNum * 10 + LastNum.

process_file([Head|_], Sum) :-
	\+process_line(Head, Sum),
	Sum = 0.

process_file([Head|Tail], TotalSum) :-
	process_line(Head, LineSum),
	process_file(Tail, TailSum),
	TotalSum is LineSum + TailSum.

file_to_lines(InputFile, LinesOut) :- 
	read_file_to_string(InputFile, Input, []),
	split_string(Input, '\n', '', LinesOut).

main :-
	file_to_lines('./input_02', Lines),
	process_file(Lines, Sum),
	write(Sum), nl,
	halt.

:- initialization(main).
