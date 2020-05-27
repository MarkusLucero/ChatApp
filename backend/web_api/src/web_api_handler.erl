%%@author Per Alonso <erlang@osundalivsval.org>
%%@doc This cowboy handler module provides callback functions necessary for the websocket connections to Chat Up!

-module(web_api_handler).
-behavior(cowboy_handler).

-export([init/2, terminate/3, websocket_init/1, websocket_handle/2, websocket_info/2]).

-spec init(Req, State) -> {ok, Req, State} | {cowboy_websocket, Req, State, map()} when
      Req :: cowboy_req:req(),
      State :: any().
%% @doc Initialize websocket connection (by forcing upgrade if contacted by other protocol)
%% @param Req0 The request sent to the server.
%% @param State The current state of the handler.
%% @returns A cowboy websocket connection if client can handle it, otherwise a 400 http response.        
init(Req0, State) ->
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
        undefined ->
            {cowboy_websocket, Req0, State, #{idle_timeout => 600000}};
        Subprotocols ->
            case lists:keymember(<<"mqtt">>, 1, Subprotocols) of
                true ->
                    Req = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                                                     <<"mqtt">>, Req0),
                    {cowboy_websocket, Req, State, #{idle_timeout => 600000}};
                false ->
                    Req = cowboy_req:reply(400, Req0),
                    {ok, Req, State}
            end
    end.

-spec terminate(Reason, Req, State) -> ok when
      Reason     :: normal | stop | timeout
                  | remote | {remote, cow_ws:close_code(), binary()}
                  | {error, badencoding | badframe | closed | atom()}
                  | {crash, error | exit | throw, any()},
      Req :: cowboy_req:req(),
      State :: any().
%% @doc Terminates websocket connection
%% @param Reason The reason for termination.
%% @param Req The request to the server.
%% @param State The state of the handler.
%% @returns ok For all terminations.
terminate(_Reason, _Req, _State) ->
    chat_server ! {dead_connection, self()},
    ok.



-spec websocket_init(State) -> {reply, {atom(), any()}, State, hibernate} when
      State :: any().
%% @doc Initializes a websocket connection
%% @param State The state of the handler
%% @returns A connection acceptance message for the client.
websocket_init(State) ->
    chat_server:new_connection(self()),
    {reply, {text, <<"Welcome">>}, State, hibernate}.

-spec websocket_handle(Data, State) -> {list(), State, hibernate} when
      State :: any(),
      Data :: ping | pong | {text | binary | ping | pong, binary()}.
%% @doc Handles messages from a websocket connection
%% @param Packet The packet from the client.
%% @param State The state of the handler.
%% @returns An ACK-message for the client if the request was valid, nothing otherwise.
websocket_handle({text, Msg}, State) ->
    io:format("Received: ~s~n", [Msg]),
    message_parser:handle_message(Msg, self()),
    {[{text, <<"ACK">>}], State, hibernate};
websocket_handle(_Data, State) ->
    {[], State, hibernate}.

-spec websocket_info(Data, State) -> {any(), State, hibernate} when
      State :: any(),
      Data :: any().
%% @doc Sends out data to the client
%% @param Info What has been requested to be sent out
%% @param State the state of the handler
%% @returns An appropriate message for the client.
websocket_info({timeout, _Ref, Msg}, State) ->
    erlang:start_timer(1000, self(), <<"Are you still there?">>),
    {[{text, Msg}], State, hibernate};
websocket_info({text, Text}, State) ->
    io:format("SENDING MESSAGE TO: ~p~n", [self()]),
    {reply, {text, Text}, State, hibernate};
websocket_info(_Info, State) ->
    {[], State, hibernate}.
