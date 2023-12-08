#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

chars_stat([], stat{}).
chars_stat([HeadChar|TailChar], HeadStat) :-
	chars_stat(TailChar, TailStat),
	NewCnt is TailStat.get(HeadChar, 0) + 1,
	HeadStat = TailStat.put(HeadChar, NewCnt).

hand_type([Head|_], Stat, "five of a kind") :- Stat.get(Head) =:= 5, !.

hand_type(List, Stat, "four of a kind") :-
	list_to_set(List, [X, Y]),
	abs(Stat.get(X) - Stat.get(Y)) =:= 3, !.

hand_type(List, Stat, "full house") :-
	list_to_set(List, [X, Y]),
	abs(Stat.get(X) - Stat.get(Y)) =:= 1, !.

hand_type(List, Stat, "three of a kind") :-
	list_to_set(List, [X, Y, Z]),
	(
		Stat.get(X) =:= 3;
		Stat.get(Y) =:= 3;
		Stat.get(Z) =:= 3
	), !.

hand_type(List, Stat, "two pair") :-
	list_to_set(List, [X, Y, Z]),
	(
		Stat.get(X) =:= 1;
		Stat.get(Y) =:= 1;
		Stat.get(Z) =:= 1
	), !.

hand_type(List, _, "one pair") :- list_to_set(List, [_, _, _, _]), !.

hand_type(List, _, "high card") :- list_to_set(List, [_, _, _, _, _]), !.

hand_type(Hand, Type) :-
	string_codes(Hand, Codes),
	chars_stat(Codes, Stat),
	hand_type(Codes, Stat, Type).

is_stronger('=', [], []).
is_stronger(Delta, [H1|T1], [H2|T2]) :-
	string_codes("23456789TJQKA", LabelOrder),
	nth0(LabelNum1, LabelOrder, H1),
	nth0(LabelNum2, LabelOrder, H2),
	(
		LabelNum1 =:= LabelNum2 ->
		is_stronger(Delta, T1, T2)
	;
		LabelNum1 > LabelNum2 ->
		Delta = '>'
	;
		Delta = '<'
	).

is_stronger(Delta, Hand1, Hand2) :-
	string(Hand1),
	string(Hand2),
	TypeOrder = ["five of a kind", "four of a kind", "full house", "three of a kind", "two pair", "one pair", "high card"],
	
	hand_type(Hand1, Type1),
	hand_type(Hand2, Type2),
	nth0(TypeNum1, TypeOrder, Type1),
	nth0(TypeNum2, TypeOrder, Type2),

	(
		TypeNum1 =:= TypeNum2 ->
		string_codes(Hand1, Codes1),
		string_codes(Hand2, Codes2),
		is_stronger(Delta, Codes1, Codes2)
	;
		TypeNum1 < TypeNum2 ->
		Delta = '>'
	;
		Delta = '<'
	).

calc([], _, 0).
calc([HeadHand|TailHand], HeadRank, HeadBid) :-
	TailRank is HeadRank + 1,
	calc(TailHand, TailRank, TailBid),
	hand_to_bid(HeadHand, Bid),
	HeadBid is TailBid + Bid * HeadRank.
	
main :-
        read_file_to_string('./input_01', File, []),
	split_string(File, '\n', '\n', Lines),
	findall(Hand, (
		member(Row, Lines),
		split_string(Row, " ", " ", [Hand, BidStr]),
		number_string(Bid, BidStr),
		assertz(hand_to_bid(Hand, Bid))
	), Hands),

	predsort(is_stronger, Hands, HandsSorted),
	calc(HandsSorted, 1, Out),
	write(Out), nl,

        halt.

:- initialization(main).
