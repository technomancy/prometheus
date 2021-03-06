-module(prometheus).

-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% our stuff
-export([start_link/0]).



%% internal

reply([<<"on">>]) ->
    prometheus_regulator:set(on),
    "Override: on";
reply([<<"off">>]) ->
    prometheus_regulator:set(off),
    "Override: off";
reply([<<"temp">>]) ->
    io_lib:format("Temperature is ~.2f", [prometheus_sensor:get()]);
reply([<<"temp">>, TempString]) ->
    {Temp, _} = string:to_integer(binary:bin_to_list(TempString)),
    prometheus_regulator:set(Temp),
    string:concat("Setting temp: ", binary:bin_to_list(TempString));
reply([<<"in">>, Minutes, <<"minutes">>, TempString]) ->
    {Temp, _} = string:to_integer(binary:bin_to_list(TempString)),
    erlang:start_timer(Minutes * 60000, self(), {set, Temp}),
    "you got it.";
reply(_) ->
    "mkay...".

reply_packet(Packet, Body) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, <<"from">>, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, <<"to">>, From),
    TmpPacket3 = exmpp_message:set_body(TmpPacket2, Body),
    exmpp_xml:remove_attribute(TmpPacket3, <<"id">>).



%% otp

init([]) ->
    Session = exmpp_session:start(),
    {ok, JID} = application:get_env(prometheus, jid),
    {ok, Password} = application:get_env(prometheus, password),
    [User, JidServer] = string:tokens(JID, "@"),
    MyJID = exmpp_jid:make(User, JidServer, "prometheus"),
    exmpp_session:auth_basic_digest(Session, MyJID, Password),
    {ok, Server} = application:get_env(prometheus, server),
    {ok, Status} = application:get_env(prometheus, status),
    {ok, _StreamId} = exmpp_session:connect_TCP(Session, Server, 5222),
    exmpp_session:login(Session),
    SetStatus = exmpp_presence:set_status(exmpp_presence:available(), Status),
    exmpp_session:send_packet(Session, SetStatus),
    {ok, Session}.

handle_info(Message=#received_packet{packet_type=iq, raw_packet=IQ}, Session)
  when Message#received_packet.queryns == 'urn:xmpp:ping' ->
    NS = exmpp_xml:get_ns_as_atom(exmpp_iq:get_payload(IQ)),
    Reply = exmpp_xml:element(NS, 'response', [], [{xmlcdata,<<"PONG">>}]),
    Result = exmpp_iq:result(IQ, exmpp_xml:element(NS, 'query', [], [Reply])),
    exmpp_component:send_packet(Session, Result),
    {noreply, Session};

handle_info(#received_packet{packet_type=message, raw_packet=P,
                             type_attr=Type}, Session)
  when Type =/= "error" ->
    case exmpp_message:get_body(P) of
        %% Don't care about typing notifications, etc.
        undefined -> ok;
        Body ->
            io:format("Received: ~p~n", [binary:bin_to_list(Body)]),
            ReplyText = reply(binary:split(Body, [<<" ">>], [])),
            exmpp_session:send_packet(Session, reply_packet(P, ReplyText))
    end,
    {noreply, Session};

% don't care about presence
handle_info(#received_packet{packet_type=presence}, Session) ->
    {noreply, Session};

handle_info({set, Temp}, Session) ->
    prometheus_regulator:set(Temp),
    {noreply, Session};

handle_info(Message, Session) ->
    io:format("unexpected handle_info: ~p", [Message]),
    {noreply, Session}.

handle_call(Message, From, Session) ->
    io:format("unexpected handle_call: ~p ~p", [Message, From]),
    {noreply, Session}.

handle_cast(Message, Session) ->
    io:format("unexpected handle_cast: ~p", [Message]),
    {noreply, Session}.

terminate(Reason, _Session) ->
    io:format("terminated: ~p", [Reason]),
    ok.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.



%% api

start_link() ->
    gen_server:start_link(?MODULE, [], []).
