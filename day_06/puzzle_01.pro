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

main :-
        read_file_to_string('./input_01', File, []),
	split_string(File, '\n', '\n', Lines),
	findall(X, (
		member(Row, Lines),
		split_string(Row, ":", " ", [_|[Tail]]),
		split_string(Tail, " ", " ", X)
	), [Time, Dist]),
	lines_to_pairs(Time, Dist, Pairs),

	process_race(Pairs, Out),

	write(Out), nl,
        halt.

:- initialization(main).
