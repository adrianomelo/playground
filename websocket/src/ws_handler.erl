-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

% https://ninenines.eu/docs/en/cowboy/2.0/guide/ws_handlers/

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	io:fwrite("adding ~p~n", [self()]),
	clients:add(self()),
	{ok, State}.

websocket_handle({text, <<"send|uieng|", Msg/binary>>}, State) ->
	io:fwrite("sending to uieng message ~p~n", [Msg]),
	clients:send({self(), Msg}),
	{ok, State};

websocket_handle({text, <<"send|host|", Msg/binary>>}, State) ->
	io:fwrite("sending to host message ~p~n", [Msg]),
	clients:send({self(), Msg}),
	{ok, State};

websocket_handle(Data, State) ->
	io:fwrite("websocket_handle: ~p, ~p~n", [self(), Data]),
	{ok, State}.

websocket_info({Pid, Msg}, State) ->
	Self = self(),
	case Pid of
		Self ->
			{ok, State};

		_ ->
			{reply, {text, Msg}, State}
	end;

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
  io:fwrite("terminate: ~p~n", [self()]),
	clients:remove(self()),
  ok.