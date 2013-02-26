%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    exoio set up and start
%%% @end
%%% Created : 17 Dec 2012 by Tony Rogvall <tony@rogvall.se>

-module(exoio).

-export([start/0, start/1]).
-export([setup/1]).

-include("analog.hrl").

start() ->
    application:start(gproc),
    application:start(kvdb),
    exoport:start().


start(DeviceID) ->
    not_yet.

setup(DeviceID) ->
    setup(DeviceID,
	  <<2,0,0,0,0,0,0,0>>,    %% Client->Server key
	  <<1,0,0,0,0,0,0,0>>).   %% Server->Client key

setup(DeviceID,DeviceKey,ServerKey) ->
    private_write(<<"ck">>,  DeviceKey),
    private_write(<<"sk">>,  ServerKey),    

    %% create analog spec 1
    
    write_analog(1, [
		     {<<"name">>, "temp"},
		     {<<"trigger-selection">>, 16#1F},
		     {<<"scale">>, 1},
		     {<<"offset">>, 0},
		     {<<"upper-limit">>, 1000},
		     {<<"lower-limit">>, 500},
		     {<<"delta">>, 10},
		     {<<"negative-delta">>, 5},
		     {<<"positive-delta">>, 3},
		     {<<"sample-frequency">>, 
		      decimal64:from_float(1.0, 2)},
		     {<<"max-transmit-frequency">>,
		      decimal64:from_float(0.2, 2)}]).

write_analog(I, Kvs) ->
    Pfx = [<<"io">>,{<<"analog">>,I}],
    lists:foreach(
	fun({K,V}) ->
		Key = kvdb_conf:join_key(Pfx++[K]),
		kvdb_conf:write({Key,[],V})
	end, Kvs).


private_write(Key, Value) ->
    kvdb_conf:write({private_key(Key), [], Value}).

private_key(Key) ->
    kvdb_conf:join_key(<<"private">>, to_binary(Key)).

to_binary(Value) when is_atom(Value) ->
    erlang:atom_to_binary(Value, latin1);
to_binary(Value) when is_list(Value) ->
    iolist_to_binary(Value);
to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value, 10)).



