%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> implements a simple XMPP echo client.
%%
%% <p>
%% This is a example use of the exmpp framework.
%% </p>
%%
%% <p>
%% Usage:
%% </p>
%% <pre>{ok, session} = echo_client:start().
%% echo_client:stop(Session).</pre>

-module(echo_client).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/0, start/2, start/3, stop/1]).
-export([init/2, init/3]).

start() ->
    start("echo@localhost", "password").

start(JID, Password) ->
    spawn(?MODULE, init, [JID, Password]).

start(JID, Password, Server) ->
    spawn(?MODULE, init, [JID, Password, Server]).

stop(EchoClientPid) ->
    EchoClientPid ! stop.

init(JID, Password) ->
    [_, ConnectServer] = string:tokens(JID, "@"),
    init(JID, Password, ConnectServer).

init(JID, Password, ConnectServer) ->
    application:start(exmpp),
    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    MySession = exmpp_session:start(),
    %% Create XMPP ID (Session Key):
    [User, Server] = string:tokens(JID, "@"),
    MyJID = exmpp_jid:make(User, Server, random),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, Password),
    %% Connect in standard TCP:
    {ok, _StreamId} = exmpp_session:connect_TCP(MySession, ConnectServer, 5222),
    exmpp_session:login(MySession),
    exmpp_session:send_packet(MySession,
                              exmpp_presence:set_status(
                                exmpp_presence:available(), "Echo Ready")),
    loop(MySession).



%% Process exmpp packet:
loop(MySession) ->
    receive
        stop ->
            exmpp_session:stop(MySession);
        %% If we receive a message, we reply with the same message
        Record = #received_packet{packet_type=message,
                                  raw_packet=Packet,
                                  type_attr=Type} when Type =/= "error" ->
            io:format("Received Message stanza:~n~p~n~n", [Record]),
            echo_packet(MySession, Packet),
            loop(MySession);
        %% If we receive a presence stanza, handle it
        Record when Record#received_packet.packet_type == 'presence' ->
            io:format("Received Presence stanza:~n~p~n~n", [Record]),
            handle_presence(MySession, Record, Record#received_packet.raw_packet),
            loop(MySession);
        %% Reply to pings
        Record when Record#received_packet.packet_type == 'iq' ->
        %% Record#received_packet.queryns == 'urn:xmpp:ping'
            io:format("Received Ping stanza:~n~p~n~n", [Record]),
            handle_ping(MySession, Record, Record#received_packet.raw_packet),
            loop(MySession);
        %% Just print unknown stanzas
        Record ->
            io:format("Received a stanza:~n~p~n~n", [Record]),
            loop(MySession)
    end.

%% Send the same packet back for each message received
echo_packet(MySession, Packet) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, <<"from">>, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, <<"to">>, From),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, <<"id">>),
    exmpp_session:send_packet(MySession, NewPacket).

handle_presence(Session, Packet, _Presence) ->
    case exmpp_jid:make(_From = Packet#received_packet.from) of
        JID ->
            case _Type = Packet#received_packet.type_attr of
                "available" ->
                    %% handle presence availabl
                    ok;
                "unavailable" ->
                    %% handle presence unavailable
                    ok;
                "subscribe" ->
                    presence_subscribed(Session, JID),
                    presence_subscribe(Session, JID);
                "subscribed" ->
                    presence_subscribed(Session, JID),
                    presence_subscribe(Session, JID)
            end
    end.

handle_ping(Session, Packet, _Ping) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, <<"from">>, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, <<"to">>, From),
    NewPacket = exmpp_xml:set_attribute(TmpPacket, <<"type">>, "result"),
    exmpp_session:send_packet(Session, NewPacket).

presence_subscribed(Session, Recipient) ->
    Presence_Subscribed = exmpp_presence:subscribed(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
    exmpp_session:send_packet(Session, Presence).

presence_subscribe(Session, Recipient) ->
    Presence_Subscribe = exmpp_presence:subscribe(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
    exmpp_session:send_packet(Session, Presence).

process_received_iq(Session,
            ) ->
    %% get namespace of IQ stanza
    NS = exmpp_xml:get_ns_as_atom(exmpp_iq:get_payload(IQ)),
    %% set text value of IQ stanza
    Reply = exmpp_xml:element(NS, 'response', [],
                                      [{xmlcdata,<<"your iq has been received">>}]);
    %% build result packet
    Result = exmpp_iq:result(IQ, exmpp_xml:element(NS, 'query', [], [Reply]));
    %% sends new packet
    exmpp_component:send_packet(Session, Result).
