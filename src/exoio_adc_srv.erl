%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Analog processing loop
%%% @end
%%% Created : 17 Dec 2012 by Tony Rogvall <tony@rogvall.se>

-module(exoio_adc_srv).

-include("analog.hrl").

-export([start/1, init/1]).
-export([loop/4]).

start(Index) ->
    spawn_link(fun() -> init(Index) end).

init(Index) ->
    C = #analog_config { 
	   index = Index,
	   name  = "temp",
	   trigger = ?LIMIT_BITS bor ?DELTA_BITS,  %% trigger on everything
	   upper_limit = 1000,
	   lower_limit = 500,
	   delta = 10,
	   negative_delta = 5,
	   positive_delta = 3,
	   sample_frequency = 1.0,         %% 10.0,
	   max_transmit_frequency = 0.2    %% once every 5s
	  },
    InhibitUs = trunc(1000000.0/C#analog_config.max_transmit_frequency),
    A = #analog { value=0,
		  scaled_value = 0,
		  timestamp = timestamp_us(),
		  inhibit_us = InhibitUs
		},
    Tu = 1000000/C#analog_config.sample_frequency,
    Td = 0,
    Ref = erlang:start_timer(trunc((Tu-Td)/1000), self(), sample),
    loop(A, C, Tu, Ref).

loop(A0,C,Tu,Ref) ->
    receive
	{timeout,Ref,sample} ->
	    A1 = run(A0,C),
	    Td = (A1#analog.timestamp - A0#analog.timestamp) - Tu,
	    T = trunc((Tu-Td)/1000),
	    io:format("Tu:~w,Td:~w,T:~w\n", [Tu,Td,T]),
	    Ref1 = erlang:start_timer(T, self(), sample),
	    loop(A1,C,Tu,Ref1);
	stop ->
	    ok
    end.

run(A,C) ->
    Value = read_sample(C#analog_config.index),
    Now = timestamp_us(),
    run(Value,A,C,Now).

run(Value,A,C,Now) ->
    ScaledValue = Value*C#analog_config.scale + C#analog_config.offset,
    Delta = ScaledValue - A#analog.scaled_value,
    %% calculate trigger bits
    Mask =
	if ScaledValue > C#analog_config.upper_limit -> ?UPPER_LIMIT_EXCEEDED;
	   true -> 0
	end bor
	if ScaledValue =< C#analog_config.lower_limit -> ?BELOW_LOWER_LIMIT;
	   true -> 0
	end bor
	if Delta > 0 ->
		if Delta > C#analog_config.delta ->
			?CHANGED_BY_MORE_THAN_DELTA;
		   true -> 0
		end bor
		if
		    Delta > C#analog_config.positive_delta ->
			?CHANGED_BY_MORE_THAN_POSITIVE_DELTA;
		    true -> 0
		end;
	   Delta < 0 ->
		if Delta < -C#analog_config.delta ->
			?CHANGED_BY_MORE_THAN_DELTA;
		   Delta < -C#analog_config.negative_delta ->
			?CHANGED_BY_MORE_THAN_NEGATIVE_DELTA;
		   true ->
			0
		end;
	   true ->
		0
	end,

    %% Delta changes always trigger, but level changes only trigger
    %% after the levels have been reset
    Mask1 = Mask band C#analog_config.trigger,

    Latch = (Mask1 band ?DELTA_BITS =/= 0) 
	orelse
	  ((Mask1 band ?UPPER_LIMIT_EXCEEDED =/= 0)
	   andalso
	     (A#analog.mask band ?UPPER_LIMIT_EXCEEDED =:= 0))
	orelse
	  ((Mask1 band ?BELOW_LOWER_LIMIT =/= 0)
	   andalso
	     (A#analog.mask band ?BELOW_LOWER_LIMIT =:= 0)),

    if Latch, Now > A#analog.inhibit ->
	    io:format("Output: ~w, trigger=~p\n", [Value,get_trigger(Mask1)]),
	     T1 = Now+A#analog.inhibit_us,
	    A#analog { value = Value,
		       scaled_value = ScaledValue,
		       mask = Mask,
		       timestamp = Now,
		       inhibit = T1 };
       true ->
	    A#analog { value = Value,
		       scaled_value = ScaledValue,
		       mask = Mask,
		       timestamp = Now }
    end.

%% read sample
read_sample(_Index) ->
    %% exoio_adc:read_sample(C#analog_config.index)
    trunc(randomex:normal(750.0, 200.0)).
%%  random:uniform(1500).

get_trigger(Mask) ->
    select_bits(Mask, ['upper-limit-exceeded',
		       'below-lower-limit',
		       'changed-by-more-than-delta',
		       'changed-by-more-than-negative-delta',
		       'changed-by-more-than-positive-delta']).

select_bits(0, _) ->
    [];
select_bits(Mask, [Name|Names]) when Mask band 1 =:= 1 ->
    [Name | select_bits(Mask bsr 1, Names)];
select_bits(Mask, [_|Names]) ->
    select_bits(Mask bsr 1, Names).


%% microsecond timestamp
timestamp_us() ->
    timestamp_us(os:timestamp()).

timestamp_us({M,S,U}) ->
    ((M*1000000+S)*1000000 + U).

