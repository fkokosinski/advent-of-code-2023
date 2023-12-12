#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).
:- use_module(library(clpfd)).

parse_lines([], []).
parse_lines([HeadLine|TailLine], [[ConditionStr, ConditionNum]|TailOut]) :-
	split_string(HeadLine, " ", " ", [ConditionStr, Condition]),
	split_string(Condition, ",", ",", ConditionNumStr),
	findall(Num, (member(Str, ConditionNumStr), number_string(Num, Str)), ConditionNum),
	parse_lines(TailLine, TailOut).

constraint_forbidden(X, Y, F) :- (F #< X) #\/ (Y #< F).

process_line([], _, []).
process_line([HeadNum|TailNum], Str, [HeadOut|TailOut]) :-
	process_line(TailNum, Str, TailOut),

	% constrait for interval length
	[X, Y] = HeadOut,
	Y #= X + HeadNum - 1,

	% constraint for forbidden positions
	string_codes(Str, Codes),
	findall(Idx, nth0(Idx, Codes, 46), Forbidden),
	(
		Forbidden \= [] ->
		maplist(constraint_forbidden(X, Y), Forbidden)
	;
		true
	),

	% constraings for interval min/max position
	string_length(Str, Len),
	Y #< Len,
	X #>= 0,

	% constrait in relation to the previous interval
	(
		TailNum \= [] ->
		[[LastX, _LastY]|_] = TailOut,
		Y #< LastX - 1
	;
		true
	),

	indomain(X),
	indomain(Y).

check_required([], _).
check_required([HeadReq|TailReq], Intervals) :-
	findall(1, (member([X, Y], Intervals), X =< HeadReq, HeadReq =< Y), Reqs),
	(
		Reqs \= [],
		check_required(TailReq, Intervals)
	;
		fail
	).

process_lines([], Sum, Sum).
process_lines([HeadLine|TailLine], HeadSum, Sum) :-
	% check required positions
	[String, Numbers] = HeadLine,
	string_codes(String, Codes),
	findall(Idx, nth0(Idx, Codes, 35), Required),

	% get possible intervals
	findall(Out, (process_line(Numbers, String, Out), check_required(Required, Out)), Intervals), !,

	% count score
	length(Intervals, Count),
	TailSum is Count + HeadSum,
	process_lines(TailLine, TailSum, Sum).

main :-
	read_file_to_string('./input_01', File, []),
	split_string(File, '\n', '\n', LinesIn),
	parse_lines(LinesIn, LinesOut),
	process_lines(LinesOut, 0, Out),

	write(Out), nl,

        halt.

:- initialization(main).
