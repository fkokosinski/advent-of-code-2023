#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

% check if the symbol is not a dot and not a number
valid_symbol(Sym) :-
	string_codes('*', [Dot|_]),
	Sym =:= Dot.

get_nth0(Pos, File, Sym) :-
	string_codes(File, FileCodes),
	nth0(Pos, FileCodes, Sym).

get_symbol_up(File, SelfPos, DimX, UpPos) :-
	UpPos is SelfPos - DimX,
	get_nth0(UpPos, File, Sym),
	valid_symbol(Sym).

get_symbol_down(File, SelfPos, DimX, DownPos) :-
	DownPos is SelfPos + DimX,
	get_nth0(DownPos, File, Sym),
	valid_symbol(Sym).

get_symbol_left(File, SelfPos, LeftPos) :-
	LeftPos is SelfPos - 1,
	get_nth0(LeftPos, File, Sym),
	valid_symbol(Sym).

get_symbol_right(File, SelfPos, RightPos) :-
	RightPos is SelfPos + 1,
	get_nth0(RightPos, File, Sym),
	valid_symbol(Sym).

get_symbol_up_left(File, SelfPos, DimX, OutPos) :-
	LeftPos is SelfPos - 1,
	get_symbol_up(File, LeftPos, DimX, OutPos).

get_symbol_down_left(File, SelfPos, DimX, OutPos) :-
	LeftPos is SelfPos - 1,
	get_symbol_down(File, LeftPos, DimX, OutPos).

get_symbol_up_right(File, SelfPos, DimX, OutPos) :-
	RightPos is SelfPos + 1,
	get_symbol_up(File, RightPos, DimX, OutPos).

get_symbol_down_right(File, SelfPos, DimX, OutPos) :-
	RightPos is SelfPos + 1,
	get_symbol_down(File, RightPos, DimX, OutPos).

is_top_row(Pos, DimX) :- Pos < DimX.

is_bottom_row(Pos, DimX, DimY) :- (DimY - 1) * DimX < Pos.

is_first_column(Pos, DimX) :- Pos mod DimX =:= 0.

is_last_column(Pos, DimX) :- Pos mod DimX =:= DimX - 1.

is_pos_valid(File, Pos, DimX, DimY, PosStar) :-
	(
		\+is_top_row(Pos, DimX),
		get_symbol_up(File, Pos, DimX, PosStar)
	);
	(
		\+is_bottom_row(Pos, DimX, DimY),
		get_symbol_down(File, Pos, DimX, PosStar)
	);
	(
		\+is_first_column(Pos, DimX),
		get_symbol_left(File, Pos, PosStar)
	);
	(
		\+is_last_column(Pos, DimX),
		get_symbol_right(File, Pos, PosStar)
	);
	(
		\+is_top_row(Pos, DimX),
		\+is_first_column(Pos, DimX),
		get_symbol_up_left(File, Pos, DimX, PosStar)
	);
	(
		\+is_bottom_row(Pos, DimX, DimY),
		\+is_first_column(Pos, DimX),
		get_symbol_down_left(File, Pos, DimX, PosStar)
	);
	(
		\+is_top_row(Pos, DimX),
		\+is_last_column(Pos, DimX),
		get_symbol_up_right(File, Pos, DimX, PosStar)
	);
	(
		\+is_bottom_row(Pos, DimX, DimY),
		\+is_last_column(Pos, DimX),
		get_symbol_down_right(File, Pos, DimX, PosStar)
	).

check_pos_valid(_, [], _, _, _) :- !, false.
check_pos_valid(File, [HeadPos|TailPos], DimX, DimY, Pos) :-
	(
		is_pos_valid(File, HeadPos, DimX, DimY, Pos);
		check_pos_valid(File, TailPos, DimX, DimY, Pos)
	).

get_numbers(File, []) :- \+re_matchsub('\\d+', File, _).
get_numbers(File, [NumHead|NumTail]) :-
	re_matchsub('\\d{1,3}', File, _{0:NumHead}),
	re_split('\\d{1,3}', File, [_|[_|[_|SplitTail]]]),
	atomics_to_string(SplitTail, ',', FileTail),
	get_numbers(FileTail, NumTail).

create_filler(0, '').
create_filler(N, Out) :-
	NTail is N - 1,
	create_filler(NTail, OutTail),
	string_concat('.', OutTail, Out).

get_num_pos(0, _, []).
get_num_pos(Len, Pos, [PosHead|PosTail]) :-
	Len > 0,
	TailLen is Len - 1,
	PosHead is Pos + TailLen,
	get_num_pos(TailLen, Pos, PosTail).

process_numbers(_, _, _, [], X, Y) :-
	X = gears{},
	Y = count{}.

process_numbers(HeadFile, DimX, DimY, [HeadNum|TailNum], HeadSum, HeadCount) :-
	% get number length
	string_length(HeadNum, HeadNumStrLen),

	% get number positions
	re_matchsub(HeadNum, HeadFile, _{0:MatchCompound}, [capture_type(range)]),
	format(atom(Match), '~w', MatchCompound),
	split_string(Match, '-', ' ', [NumStartStr, NumLenStr]),
	number_string(NumStart, NumStartStr),
	number_string(NumLen, NumLenStr),
	get_num_pos(NumLen, NumStart, NumPos),
	check_pos_valid(HeadFile, NumPos, DimX, DimY, Pos),

	% create replace string and replace the number in the main string; put
	% that as the new arg
	create_filler(HeadNumStrLen, Replace),
	re_replace(HeadNum, Replace, HeadFile, TailFile),
	process_numbers(TailFile, DimX, DimY, TailNum, TailSum, TailCount),

	number_string(Num, HeadNum),
	OldVal = TailSum.get(Pos, 1),
	NewVal is OldVal * Num,
	HeadSum = TailSum.put(Pos, NewVal),

	OldCount = TailCount.get(Pos, 0),
	NewCount is OldCount + 1,
	HeadCount = TailCount.put(Pos, NewCount).

process_numbers(HeadFile, DimX, DimY, [HeadNum|TailNum], HeadSum, HeadCount) :-
	% get number length
	string_length(HeadNum, HeadNumStrLen),

	create_filler(HeadNumStrLen, Replace),
	re_replace(HeadNum, Replace, HeadFile, TailFile),
	process_numbers(TailFile, DimX, DimY, TailNum, TailSum, TailCount),
	HeadSum = TailSum,
	HeadCount = TailCount.

sum_dict(_, [], 0).
sum_dict(Dict, [HeadIdx|TailIdx], HeadSum) :-
	Gear = Dict.get(HeadIdx),
	sum_dict(Dict, TailIdx, TailSum),
	HeadSum is Gear + TailSum.

main :-
	% read the input file into a single string
	read_file_to_codes('./input_02', File, []),

	% find the X dimension of the file; cutoff is required so that only the
	% first newline position is reported
	string_codes('\n', [Nl|_]),
	nth0(DimX, File, Nl), !,

	% remove newlines from the file
	delete(File, Nl, FileFiltered),

	% calculate the Y dimension
	length(FileFiltered, Len),
	DimY is Len // DimX,

	% find all numbers
	string_codes(FileStr, FileFiltered),
	get_numbers(FileStr, Nums),

	process_numbers(FileStr, DimX, DimY, Nums, Sum, Count),
	findall(X, (Cnt = Count.get(X), Cnt > 1), Bag),
	sum_dict(Sum, Bag, OutSum),
	write(OutSum), nl,
	halt.

:- initialization(main).
