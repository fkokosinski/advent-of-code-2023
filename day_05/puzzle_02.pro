#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

add_rules(Out, In, Range, Type) :-
	Difference is Out - In,
	StartRange is In,
	EndRange is In + Range - 1,
	(
		Type = "seed-to-soil map:" ->
		assertz(seed_to_soil(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference)),
		assertz(seed_to_soil_range([StartRange, EndRange]))
	;
		Type = "soil-to-fertilizer map:" ->
		assertz(soil_to_fertilizer(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference)),
		assertz(soil_to_fertilizer_range([StartRange, EndRange]))
	;

		Type = "fertilizer-to-water map:" ->
		assertz(fertilizer_to_water(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference)),
		assertz(fertilizer_to_water_range([StartRange, EndRange]))
	;

		Type = "water-to-light map:" ->
		assertz(water_to_light(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference)),
		assertz(water_to_light_range([StartRange, EndRange]))
	;

		Type = "light-to-temperature map:" ->
		assertz(light_to_temperature(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference)),
		assertz(light_to_temperature_range([StartRange, EndRange]))
	;

		Type = "temperature-to-humidity map:" ->
		assertz(temperature_to_humidity(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference)),
		assertz(temperature_to_humidity_range([StartRange, EndRange]))
	;

		Type = "humidity-to-location map:" ->
		assertz(humidity_to_location(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference)),
		assertz(humidity_to_location_range([StartRange, EndRange]))
	).


process_mapping([], _).
process_mapping([HeadMap|TailMap], Type) :-
	findall(X, (member(Y, HeadMap), number_string(X, Y)), [In, Out, Range]),
	add_rules(In, Out, Range, Type),
	process_mapping(TailMap, Type).

process_category([]).
process_category([HeadCat|TailCat]) :-
	[Type|Mappings] = HeadCat,

	% get mappings
	findall(X, (member(Y, Mappings), split_string(Y, ' ', ' ', X)), MapListStr),
	process_mapping(MapListStr, Type),

	process_category(TailCat).

% In x Range -> Left, Intersection, Right
interval_intersection([X1, Y1], [X2, _], [X1, Y1], [], []) :- X2 > Y1, !.
interval_intersection([X1, Y1], [_, Y2], [], [], [X1, Y1]) :- X1 > Y2, !.
interval_intersection([X1, Y1], [X2, Y2], Left, [XOut, YOut], Right) :-
	XOut is max(X1, X2),
	YOut is min(Y1, Y2), !,
	(
		X1 >= X2 ->
		Left = []
	;
		EndLeft is XOut - 1,
		Left = [X1, EndLeft]
	),
	(
		Y1 =< Y2 ->
		Right = []
	;
		StartRight is YOut + 1,
		Right = [StartRight, Y1]
	), !.

% flat list to [X, Y] list
to_pairs([], []).
to_pairs([First|[Second|Tail]], [HeadPair|TailPair]) :-
	HeadPair = [First, Second],
	to_pairs(Tail, TailPair).

% [Start, Range] list to [Start, End] list
pairs_to_seeds([], []).
pairs_to_seeds([HeadSeed|[HeadRange|TailSeed]], [[SeedStart, SeedEnd]|Tail]) :-
	SeedStart is HeadSeed,
	SeedEnd is SeedStart + HeadRange - 1,

	pairs_to_seeds(TailSeed, Tail).

get_step_predicate(Step, ConvertPredicate, RangePredicate) :-
	(
		Step = "seed" ->
		ConvertPredicate = seed_to_soil,
		RangePredicate = seed_to_soil_range
	;
		Step = "soil" ->
		ConvertPredicate = soil_to_fertilizer,
		RangePredicate = soil_to_fertilizer_range
	;
		Step = "fertilizer" ->
		ConvertPredicate = fertilizer_to_water,
		RangePredicate = fertilizer_to_water_range
	;
		Step = "water" ->
		ConvertPredicate = water_to_light,
		RangePredicate = water_to_light_range
	;
		Step = "light" ->
		ConvertPredicate = light_to_temperature,
		RangePredicate = light_to_temperature_range
	;
		Step = "temperature" ->
		ConvertPredicate = temperature_to_humidity,
		RangePredicate = temperature_to_humidity_range
	;
		Step = "humidity" ->
		ConvertPredicate = humidity_to_location,
		RangePredicate = humidity_to_location_range
	).

convert_pair([], [], _).
convert_pair([StartIn, EndIn], [StartOut, EndOut], Predicate) :-
	call(Predicate, StartIn, StartOut),
	call(Predicate, EndIn, EndOut).

get_overlaps([X, Y], [], [[X, Y]]).
get_overlaps([[]], _, []).
get_overlaps([], _, []).
get_overlaps(HeadInterval, [HeadRange|TailRange], HeadOut) :-
	interval_intersection(HeadInterval, HeadRange, Left, Intersection, Right),
	get_overlaps(Left, TailRange, TailOutLeft),
	get_overlaps(Right, TailRange, TailOutRight),

	append(TailOutLeft, TailOutRight, TailOut),
	(
		Intersection = [],
		HeadOut = TailOut
	;
		append(TailOut, [Intersection], HeadOut)
	).

process_pairs(Range, [], Min) :-
	flatten(Range, Flat),
	min_list(Flat, Min).

process_pairs([HeadInterval|TailInterval], [HeadStep|TailStep], Min) :-
	get_step_predicate(HeadStep, ConvertPredicate, RangePredicate),

	% get all ranges for this step
	findall(X, call(RangePredicate, X), Ranges),

	% get overlap ranges and convert them with translation rules
	get_overlaps(HeadInterval, Ranges, OutRanges),
	findall(X, (
		member(Range, OutRanges),
		convert_pair(Range, X, ConvertPredicate)
	), OutConvert),

	process_pairs(OutConvert, TailStep, MinDown),
	(
		TailInterval \= [] ->
		process_pairs(TailInterval, [HeadStep|TailStep], MinSide),
		Min is min(MinDown, MinSide)
	;
		Min is MinDown
	).

main :-
        read_file_to_string('./input_02', File, []),

	% get seeds
	re_matchsub('seeds: (.*)', File, _{0:_, 1:SeedsStr}, []),
	split_string(SeedsStr, ' ', ' ', SeedsList),
	findall(X, (member(Y, SeedsList), number_string(X, Y)), Pairs),
	pairs_to_seeds(Pairs, Seeds),

	% get a list of sublists with mappings
	re_split('\n\n', File, [_, _|Split]),
	findall(X, (nth0(Y, Split, X), Y mod 2 =:= 0), SplitFiltered),
	findall(X, (member(Y, SplitFiltered), split_string(Y, '\n', ' ', X)), Categories),

	% add conversion rules
	process_category(Categories),
	assertz(seed_to_soil(X, X)),
	assertz(soil_to_fertilizer(X, X)),
	assertz(fertilizer_to_water(X, X)),
	assertz(water_to_light(X, X)),
	assertz(light_to_temperature(X, X)),
	assertz(temperature_to_humidity(X, X)),
	assertz(humidity_to_location(X, X)),

	Steps = ["seed", "soil", "fertilizer", "water", "light", "temperature", "humidity"],
	process_pairs(Seeds, Steps, Out),
	write(Out), nl,
        halt.

:- 
	set_prolog_flag(stack_limit, 29_147_483_648),
	initialization(main).
