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
		assertz(seed_to_soil(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference))
	;
		Type = "soil-to-fertilizer map:" ->
		assertz(soil_to_fertilizer(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference))
	;

		Type = "fertilizer-to-water map:" ->
		assertz(fertilizer_to_water(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference))
	;

		Type = "water-to-light map:" ->
		assertz(water_to_light(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference))
	;

		Type = "light-to-temperature map:" ->
		assertz(light_to_temperature(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference))
	;

		Type = "temperature-to-humidity map:" ->
		assertz(temperature_to_humidity(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference))
	;

		Type = "humidity-to-location map:" ->
		assertz(humidity_to_location(Y, X) :- (Y >= StartRange, Y =< EndRange, !, X is Y + Difference))
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

seed_to_location(Seed, Location) :-
	!,
	seed_to_soil(Seed, Soil),
	soil_to_fertilizer(Soil, Fertilizer),
	fertilizer_to_water(Fertilizer, Water),
	water_to_light(Water, Light),
	light_to_temperature(Light, Temperature),
	temperature_to_humidity(Temperature, Humidity),
	humidity_to_location(Humidity, Location).

main :-
        read_file_to_string('./input_01', File, []),

	% get seeds
	re_matchsub('seeds: (.*)', File, _{0:_, 1:SeedsStr}, []),
	split_string(SeedsStr, ' ', ' ', SeedsList),
	findall(X, (member(Y, SeedsList), number_string(X, Y)), Seeds),

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

	findall(X, (member(Y, Seeds), seed_to_location(Y, X)), Locations),
	min_list(Locations, Out),
	write(Out), nl,
        halt.

:- initialization(main).
