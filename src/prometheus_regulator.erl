-module(prometheus_regulator).

-behaviour(gen_server).

-define(TIMEOUT, 5000).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% our stuff
-export([start_link/0, set/1]).



%% internal

write_pin(Pin, false) ->
    file:write_file(Pin, "0");
write_pin(Pin, true) ->
    file:write_file(Pin, "1").



%% otp

init([]) ->
    erlang:register(prometheus_regulator, self()),
    {ok, Relay} = application:get_env(prometheus, relay),
    {ok, Target} = application:get_env(prometheus, init_temp),
    {ok, {Relay, Target}, ?TIMEOUT}.

handle_call({set, Target}, _From, {Relay, _Target}) ->
    {reply, ok, {Relay, Target}, ?TIMEOUT};
handle_call(Message, From, State) ->
    io:format("unexpected handle_call: ~p ~p", [Message, From]),
    {noreply, State, ?TIMEOUT}.

handle_cast(Message, State) ->
    io:format("unexpected handle_cast: ~p", [Message]),
    {noreply, State, ?TIMEOUT}.

handle_info(timeout, {Relay, on}) ->
    write_pin(Relay, true),
    {noreply, {Relay, on}, ?TIMEOUT};
handle_info(timeout, {Relay, off}) ->
    write_pin(Relay, false),
    {noreply, {Relay, off}, ?TIMEOUT};
handle_info(timeout, {Relay, Target}) ->
    write_pin(Relay, prometheus_sensor:get() < Target),
    {noreply, {Relay, Target}, ?TIMEOUT};
handle_info(Message, State) ->
    io:format("unexpected handle_info: ~p", [Message]),
    {noreply, State, ?TIMEOUT}.

terminate(Reason, _State) ->
    io:format("terminated: ~p", [Reason]),
    ok.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State, ?TIMEOUT}.



%% api

set(Target) ->
    gen_server:call(prometheus_regulator, {set, Target}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).
