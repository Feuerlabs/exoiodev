%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Read write digital i/o
%%% @end
%%% Created : 13 Dec 2012 by Tony Rogvall <tony@rogvall.se>

-module(exoio_gpio).

-export([write/1]).
-export([read/1]).
-export([mode/1]).

-type pin_number() :: 1..1023.
-type uint16()     :: 0..65535.

-type write_argument() :: {pin, pin_number()} |
			  {level, uint16()}.

-spec write(Args::[write_argument()]) ->
		   {notify, 'exoio:write-callback', []}.

write(Args) ->
    Pin   = proplists:get_value(pin, Args),
    Level = proplists:get_value(level, Args),
    {notify, 'exoio:write-callback', []}.

-type read_argument() :: {pin, pin_number()}.

-spec read(Args::[read_argument()]) -> 
		  {notify, 'exoio:read-callback', [{level,integer()}]}.
read(Args) ->
    Pin   = proplists:get_value(pin, Args),
    if Pin > 32 ->
	    {notify, 'exoio:read-callback', [{level, low}]};
       true ->
	    {notify, 'exoio:read-callback', [{level, high}]}
    end.

-type mode_argument() :: 
	{pin, pin_number()} |
	{mode, output|input|input_pullup}.
			 
-spec mode(Args::[mode_argument()]) ->
		  {notify, 'exoio:mode-callback', []}.

mode(Args) ->
    Pin   = proplists:get_value(pin, Args),
    Mod   = proplists:get_value(mode, Args),
    {notify, 'exoio:write-callback', []}.
