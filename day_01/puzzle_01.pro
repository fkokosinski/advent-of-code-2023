#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).

first_number([Head|_], Num) :-
	string_codes(Char, [Head]),
	atom_number(Char, Num).

first_number([Head|Str], Num) :-
	string_codes(Char, [Head]),
	\+atom_number(Char, Num),
	first_number(Str, Num).

process_line(Line, Sum) :-
	string_codes(Line, Codes),
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
	file_to_lines('./input_01', Lines),
	process_file(Lines, Sum),
	write(Sum), nl,
	halt.

:- initialization(main).
