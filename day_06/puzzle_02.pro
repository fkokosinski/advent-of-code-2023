#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

process_race([], 1).
process_race([[TimeStr, RecordStr]|TailRace], HeadOut) :-
	number_string(Time, TimeStr),
	number_string(Record, RecordStr),

	findall(X, (
		numlist(0, Time, SpeedUps),
		member(SpeedUp, SpeedUps),
		X is SpeedUp * (Time - SpeedUp),
		X > Record
	), BeatList),
	length(BeatList, Out),

	process_race(TailRace, TailOut),
	HeadOut is TailOut * Out.

lines_to_pairs([], [], []).
lines_to_pairs([TimeHead|TimeTail], [DistHead|DistTail], [[TimeHead, DistHead]|OutTail]) :-
	lines_to_pairs(TimeTail, DistTail, OutTail).

concat_all([Str], Str).
concat_all([HeadStr|TailStr], HeadOut) :-
	concat_all(TailStr, TailOut),
	string_concat(HeadStr, TailOut, HeadOut).

main :-
        read_file_to_string('./input_02', File, []),
	split_string(File, '\n', '\n', Lines),
	findall(X, (
		member(Row, Lines),
		split_string(Row, ":", " ", [_|[Tail]]),
		split_string(Tail, " ", " ", X)
	), [Time, Dist]),

	concat_all(Time, TimeConcat),
	concat_all(Dist, DistConcat),
	lines_to_pairs([TimeConcat], [DistConcat], Pairs),

	process_race(Pairs, Out),

	write(Out), nl,
        halt.

:- 
	% increase the stack limit to 6 GiB because I'm too lazy to write an
	% optimized solution
	set_prolog_flag(stack_limit, 6_442_450_944),
	initialization(main).
