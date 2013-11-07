-module(prometheus_regulator).

-export([start/3, regulate/3, set/2, get/3]).

start(Sensor, Pin, Target) ->
    spawn(?MODULE, regulate, [Sensor, Pin, Target]).

set(Regulator, Temp) ->
    Regulator ! {set, Temp}.

get(Regulator, RequestorPid, ReplyTo) ->
    Regulator ! {get, RequestorPid, ReplyTo}.

warm_enough(Sensor, Target) ->
    case file:read_file(Sensor) of
        {ok, Temp} ->
            {Sensed, _} = string:to_integer(binary:bin_to_list(Temp)),
            Sensed >= Target;
        _ -> true
    end.

write_pin(Pin, true) ->
    file:write_file(Pin, "0");
write_pin(Pin, false) ->
    file:write_file(Pin, "1").

regulate(Sensor, Pin, Target) ->
    receive
        stop ->
            ok;
        {get, RequestorPid, ReplyTo} ->
            case file:read_file(Sensor) of
                {error,enoent} ->
                    RequestorPid ! {temp, ReplyTo, <<"Couldn't read sensor.">>};
                {ok, Temp} ->
                    RequestorPid ! {temp, ReplyTo, Temp}
            end,
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
