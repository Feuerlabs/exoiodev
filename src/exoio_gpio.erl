%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Read write digital i/o
%%% @end
%%% Created : 13 Dec 2012 by Tony Rogvall <tony@rogvall.se>

-module(exoio_gpio).

-export([write/2]).
-export([read/1]).
-export([mode/2]).

-type pin_number() :: 1..1023.

-spec write(Pin::pin_number(), Value::high | low) -> ok.

write(Pin, low) when Pin >= 0, Pin =< 1023 ->
    ok;
write(Pin, high) when Pin >= 0, Pin =< 1023 ->
    ok.

-spec read(Pin::pin_number()) -> high | low.
read(Pin) when Pin >= 0, Pin =< 1023 ->
    high.

-spec mode(Pin::pin_number(), Mode::output|input|nput_pullup) -> ok.

mode(Pin,output) when Pin >= 0, Pin =< 1023 ->
    ok;
mode(Pin,input) when Pin >= 0, Pin =< 1023 ->
    ok;
mode(Pin,input_pullup) when Pin >= 0, Pin =< 1023 ->
    ok.
