-module(prometheus_sensor).

-behaviour(gen_server).

-define(TIMEOUT, 60000).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% our stuff
-export([start_link/0, get/0]).



%% internal

temperature(Sensor) ->
    case file:read_file(Sensor) of
        {ok, TempBin} ->
            {Sensed, _} = string:to_integer(binary:bin_to_list(TempBin)),
            %% celsius reading is (v * 100) - 50
            %% ADC gives us 0-1024 out of 1.8v, so dividing by 9.21 gives us v
            (Sensed / 9.21) - 50;
        _ -> erlang:error(no_sensor)
    end.

seconds({MegaSecs,Secs,_MicroSecs}) -> (MegaSecs * 1000000 + Secs).

log_temp(Temperature, LogFile) ->
    Entry = io_lib:format("~p ~.2f~n", [seconds(erlang:now()), Temperature]),
    ok = file:write(LogFile, Entry).



%% otp

init([]) ->
    erlang:register(prometheus_sensor, self()),
    {ok, Sensor} = application:get_env(prometheus, sensor),
    {ok, Log} = application:get_env(prometheus, log),
    LogFile = file:open(Log, [append]),
    Temp = temperature(Sensor),
    {ok, {Temp, Sensor, LogFile}, ?TIMEOUT}.

handle_call(temperature, _From, {Temp, Sensor, LogFile}) ->
    {reply, Temp, {Temp, Sensor, LogFile}, ?TIMEOUT};
handle_call(Message, From, State) ->
    io:format("unexpected handle_call: ~p ~p", [Message, From]),
    {noreply, State, ?TIMEOUT}.

handle_cast(Message, State) ->
    io:format("unexpected handle_cast: ~p", [Message]),
    {noreply, State, ?TIMEOUT}.

handle_info(timeout, {_Temp, Sensor, LogFile}) ->
    Temp = temperature(Sensor),
    log_temp(Temp, LogFile),
    {noreply, {Temp, Sensor, LogFile}, ?TIMEOUT};
handle_info(Message, State) ->
    io:format("unexpected handle_info: ~p", [Message]),
    {noreply, State, ?TIMEOUT}.

terminate(Reason, {_Temp, _Sensor, LogFile}) ->
    io:format("terminated: ~p", [Reason]),
    ok = file:close(LogFile),
    ok.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State, ?TIMEOUT}.



%% api

get() ->
    gen_server:call(prometheus_sensor, temperature).

start_link() ->
    gen_server:start_link(?MODULE, [], []).
