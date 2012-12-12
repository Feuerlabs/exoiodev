%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    IO rpc module
%%% @end
%%% Created : 13 Dec 2012 by Tony Rogvall <tony@rogvall.se>

-module(exoio_adc).

-export([get_sample/1]).

get_sample(Pin) when Pin >=0, Pin =< 1023 ->
    1500.
