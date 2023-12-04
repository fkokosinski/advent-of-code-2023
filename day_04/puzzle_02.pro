#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).
:- use_module(library(dicts)).

gen_list(_, 0, []).
gen_list(CardId, Len, List) :-
	FirstId is CardId + 1,
	FinalId is CardId + Len,
	numlist(FirstId, FinalId, List).


update_dict([], [], Dict) :- Dict = count{}.
update_dict([HeadId|TailId], [HeadCount|TailCount], HeadDict) :-
	update_dict(TailId, TailCount, TailDict),
	HeadDict = TailDict.put(HeadId, HeadCount).

% lines -> [winning numbers, drawn numbers]
process_lines([_], []).
process_lines([HeadLine|TailLine], [HeadCount|TailCount]) :-
	% get card id
	re_matchsub('Card\\s+(\\d+):', HeadLine, _{0:_, 1:IdString}, []),
	number_string(Id, IdString),

	% get winning and drawn numbers
	split_string(HeadLine, ':', ' ', [_, Numbers]),
	split_string(Numbers, '|', ' ', [WinningString, DrawnString]),
	split_string(WinningString, ' ', ' ', Winning),
	split_string(DrawnString, ' ', ' ', Drawn),

	% find common numbers
	findall(X, (member(X, Winning), member(X, Drawn)), Bag),
	length(Bag, BagLen),

	% generate Ids that needs to be copied
	gen_list(Id, BagLen, HeadCount),

	% recurrence
	process_lines(TailLine, TailCount).

%max_key(Dict, Max) :-
%	dict_keys(Dict, Keys),
%	max_list(Keys, Max).

%process_counts([], _, 0).
%process_counts([HeadCount|TailCount], HeadCount, Sum) :-
%	max_key(HeadCount, LastId),
%	SelfId is LastId + 1,
%	SelfCount is HeadCount.get(SelfId, 1),
%	process_counts(TailCount, TailCount, Sum).

process_dict([], _, Dict, Dict).
process_dict([HeadCount|TailCount], Id, HeadDict, DictOut) :-
	% get self count
	SelfCount = HeadDict.get(Id),

	% add new copies
	CopyCount = HeadDict.get(HeadCount, 0),
	CopyCountIncr is CopyCount + SelfCount,
	TailDict = HeadDict.put(HeadCount, CopyCountIncr),

	% recursion
	process_dict(TailCount, Id, TailDict, DictOut).


counts_to_dict([], Dict, _, Dict).
counts_to_dict([HeadCount|TailCount], HeadDict, HeadId, OutDict) :-
	TailId is HeadId + 1,
	Count = HeadDict.get(HeadId, 0),
	SelfCount is Count + 1,

	Dict = HeadDict.put(HeadId, SelfCount),

	process_dict(HeadCount, HeadId, Dict, TailDict),
	counts_to_dict(TailCount, TailDict, TailId, OutDict).


main :-
	read_file_to_string('./input_02', File, []),
	split_string(File, '\n', ' ', Lines),
	process_lines(Lines, Counts),
	counts_to_dict(Counts, count{}, 1, Out),
	findall(X, (X = Out.get(_)), Bag),
	sum_list(Bag, Sum),

	write(Sum), nl,
	halt.

:- initialization(main).
