-module(prometheus_regulator).

-export([set/2, start/3]).

start(Sensor, Pin, Target) ->
    spawn(?MODULE, regulate, [Sensor, Pin, Target]).

set(Regulator, Temp) ->
    Regulator ! {set, Temp}.

warm_enough(Sensor, Target) ->
    string:to_integer(file:read_file(Sensor)) >= Target.

write_pin(Pin, true) ->
    file:write_file(PinPath, "1").
write_pin(Pin, false) ->
    file:write_file(PinPath, "0").

regulate(Sensor, Pin, Target) ->
    receive
        stop ->
            ok;
        {set, Temp} ->
            regulate(Sensor, Pin, Temp)
    after
        10000 ->
            write_pin(Pin, warm_enough(Sensor, Target)),
            regulate(Sensor, Pin, Target)
    end.
