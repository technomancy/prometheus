-module(temp).

-export([set/2, start/3]).

start(Sensor, Pin, Target) ->
    spawn(?MODULE, regulate, [Sensor, Pin, Target]).

set(Regulator, Temp) ->
    Regulator ! {set, Temp}.

read(Sensor) ->
    cool. %% TODO

write_pin(Pin, value) ->
    ok. %% TODO

regulate(Sensor, Pin, Target) ->
    receive
        stop ->
            ok;
        {set, Temp} ->
            regulate(Sensor, Pin, Temp)
    after
        10000 ->
            case read(Sensor) of
                cool ->
                    write_pin(Pin, "1");
                warm ->
                    write_pin(Pin, "0")
            end,
            regulate(Sensor, Pin, Target)
    end.
