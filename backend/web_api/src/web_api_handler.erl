-module(web_api_handler).
-behavior(cowboy_handler).

-export([init/2, terminate/3, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req0, State) ->
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
        undefined ->
            {cowboy_websocket, Req0, State, #{idle_timeout => 60000}};
        Subprotocols ->
            case lists:keymember(<<"mqtt">>, 1, Subprotocols) of
                true ->
                    Req = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                                                     <<"mqtt">>, Req0),
                    {cowboy_websocket, Req, State, #{idle_timeout => 60000}};
                false ->
                    Req = cowboy_req:reply(400, Req0),
                    {ok, Req, State}
            end
    end.

terminate(_Reason, _Req, _State) ->
    ok.




websocket_init(State) ->
    chat_server:start(),
    {reply, {text, <<"Welcome">>}, State}.

websocket_handle({text, Msg}, State) ->
    chat_server:add(Msg),
    List = chat_server:get(),
    {[{text, lists:flatten(List)}], State};
websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    erlang:start_timer(1000, self(), <<"Are you still there?">>),
    {[{text, Msg}], State};
websocket_info(_Info, State) ->
    {[], State}.
