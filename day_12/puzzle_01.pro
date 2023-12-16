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
constraint_forbidden_incl(X, Y, F) :- (F #=< X) #\/ (Y #=< F).
constraint_first(X, F) :- X #=< F.
constraint_last(Y, F) :- Y #>= F.

process_line([], _Req, _Str, _Sum, []).
process_line([HeadNum|TailNum], Req, Str, Sum, [HeadOut|TailOut]) :-
	process_line(TailNum, Req, Str, Sum, TailOut),

	% constraint for interval length
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


	% constraints for interval min/max position
	string_length(Str, Len),
	Y #< Len,
	sum_list(TailNum, SumNum),
	X #>= Sum - SumNum - HeadNum,

	% constraint in relation to the previous interval
	(
		TailNum \= [] ->
		[[LastX, _LastY]|_] = TailOut,
		Y #< LastX - 1,

		% constraint for required positions
		maplist(constraint_forbidden_incl(Y, LastX), Req),
		(
			% constraint for first interval
			Sum - SumNum - HeadNum =:= 0 ->
			maplist(constraint_first(X), Req)
		;
			true

		)
	;
		% constraint for last interval
		maplist(constraint_last(Y), Req)
	).

only_first([], []).
only_first([[X, _Y]|TailIntervals], [X|TailOut]) :-
	only_first(TailIntervals, TailOut).

process_lines([], Sum, Sum).
process_lines([HeadLine|TailLine], HeadSum, Sum) :-
	% check required positions
	[String, Numbers] = HeadLine,
	sum_list(Numbers, SumNum),
	string_codes(String, Codes),
	findall(Idx, nth0(Idx, Codes, 35), Required),

	% add constraints
	process_line(Numbers, Required, String, SumNum, Out),
	only_first(Out, Vars),

	% count the number of solutions
	findall(S, (labeling([upto_in(S)], Vars)), Ss), !,

	% count score
	sum_list(Ss, Count),
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
