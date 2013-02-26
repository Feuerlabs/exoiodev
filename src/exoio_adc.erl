%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    IO rpc module
%%% @end
%%% Created : 13 Dec 2012 by Tony Rogvall <tony@rogvall.se>

-module(exoio_adc).

-export([get_sample/1]).

-type pin_number() :: 1..1023.

-type get_sample_argument() :: {pin, pin_number()}.

%% get_sample is the exosense enabled RPC version
-spec get_sample(Args::[get_sample_argument()]) -> 
		  {notify, 'exoio:read-callback', [{value,integer()}]}.

get_sample(Args) ->
    Pin = proplists:get_value(pin, Args),
    {notify, 'exoio:get-sample-callback', [{value, read_sample(Pin)}]}.

%% Read a local sample from pin/channel
-spec read_sample(Pin::pin_number()) -> non_neg_integer().
read_sample(Pin) when Pin >= 1, Pin =< 1023 ->
    1550 + Pin.
