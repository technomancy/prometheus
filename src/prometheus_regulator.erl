-module(prometheus_regulator).

-export([start/3, regulate/3, set/2, get/3, temperature/1]).

start(Sensor, Pin, Target) ->
    spawn(?MODULE, regulate, [Sensor, Pin, Target]).

set(Regulator, Temp) ->
    Regulator ! {set, Temp}.

get(Regulator, RequestorPid, ReplyTo) ->
    Regulator ! {get, RequestorPid, ReplyTo}.

temperature(Sensor) ->
    case file:read_file(Sensor) of
        {ok, TempBin} ->
            {Sensed, _} = string:to_integer(binary:bin_to_list(TempBin)),
            %% celsius reading is (v * 100) - 50
            %% ADC gives us 0-1024 out of 1.8v, so dividing by 9.21 gives us v
            (Sensed / 9.21) - 50;
        _ -> erlang:error(no_sensor)
    end.

warm_enough(Sensor, Target) ->
    temperature(Sensor) >= Target.

write_pin(Pin, true) ->
    file:write_file(Pin, "0");
write_pin(Pin, false) ->
    file:write_file(Pin, "1").

regulate(Sensor, Pin, Target) ->
    receive
        stop ->
            ok;
        {get, RequestorPid, ReplyTo} ->
            RequestorPid ! {temp, ReplyTo, temperature(Sensor)},
            regulate(Sensor, Pin, Target);
        {set, Temp} ->
            io:format("Setting temp: ~p~n", [Temp]),
            regulate(Sensor, Pin, Temp)
    after
        10000 ->
            io:format("regulating to ~p: ~p.~n", [Target,
                                                  warm_enough(Sensor, Target)]),
            write_pin(Pin, warm_enough(Sensor, Target)),
            regulate(Sensor, Pin, Target)
    end.
