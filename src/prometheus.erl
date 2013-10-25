-module(prometheus).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/5, init/4, stop/1]).

start(JID, Password, Server, Sensor, Pin) ->
    Regulator = prometheus_regulator:start(Sensor, Pin, 24),
    spawn(?MODULE, init, [JID, Password, Server, Regulator]).

stop(Pid) ->
    Pid ! stop.

init(JID, Password, Server, Regulator) ->
    application:start(exmpp),
    Session = exmpp_session:start(),
    [User, JidServer] = string:tokens(JID, "@"),
    MyJID = exmpp_jid:make(User, JidServer, "prometheus"),
    exmpp_session:auth_basic_digest(Session, MyJID, Password),
    {ok, _StreamId} = exmpp_session:connect_TCP(Session, Server, 5222),
    exmpp_session:login(Session),
    exmpp_session:send_packet(Session,
                              exmpp_presence:set_status(
                                exmpp_presence:available(), "Echo Ready")),
    loop(Session, Regulator).



loop(Session, Regulator) ->
    receive
        stop ->
            Regulator ! stop,
            exmpp_session:stop(Session);
        Record = #received_packet{packet_type=message,
                                  type_attr=Type} when Type =/= "error" ->
            handle(Session, Record, Regulator),
            loop(Session, Regulator);
        %% Nothing fancy for presence
        Record when Record#received_packet.packet_type == 'presence' ->
            loop(Session, Regulator);
        %% Reply to pings
        Record when Record#received_packet.packet_type == 'iq',
        Record#received_packet.queryns == 'urn:xmpp:ping' ->
            io:format("Received Ping stanza:~n~p~n~n", [Record]),
            handle(Session, Record, Regulator),
            loop(Session, Regulator);
        %% Just print unknown stanzas
        Record ->
            io:format("Received a stanza:~n~p~n~n", [Record]),
            loop(Session, Regulator)
    end.



handle(Session, #received_packet{packet_type=iq,
                                 raw_packet=IQ}, _) ->
    NS = exmpp_xml:get_ns_as_atom(exmpp_iq:get_payload(IQ)),
    Reply = exmpp_xml:element(NS, 'response', [], [{xmlcdata,<<"PONG">>}]),
    Result = exmpp_iq:result(IQ, exmpp_xml:element(NS, 'query', [], [Reply])),
    exmpp_component:send_packet(Session, Result);

handle(Session, #received_packet{packet_type=message,
                                 raw_packet=Packet}, Regulator) ->
    io:format("Received Message:~n~p~n~n", [Packet]),
    case exmpp_message:get_body(Packet) of
        %% Don't care about typing notifications, etc.
        undefined -> ok;
        Body -> exmpp_session:send_packet(Session, reply_for(Packet,
                                                             Regulator,
                                                             Body))
    end.

reply_for(Packet, Regulator, Body) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, <<"from">>, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, <<"to">>, From),
    TmpPacket3 = exmpp_message:set_body(TmpPacket2, reply_text(Body, Regulator)),
    exmpp_xml:remove_attribute(TmpPacket3, <<"id">>).

reply_text(Body, Regulator) ->
    case re:run(Body, "^temp ([0-9]+)") of
        nomatch -> "Yeah, whatever";
        {match,[_,{Start,Length}|_]} ->
            Temp = string:substr(Body, Start+1, Length),
            prometheus_regulator:set(Regulator, string:to_integer(Temp)),
            string:concat("Setting temp: ", Temp)
    end.



%% P1 = prometheus:start("bot@hagelb.org", "password", "xmpp1.hosted.im").

%% prometheus:stop(P1). f(P1).
