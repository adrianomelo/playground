-module(websocket_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([run/0]).

run() ->
  Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, websocket, "index.html"}},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, {dir, "priv", [
				{mimetypes, cow_mimetypes, all},
				{dir_handler, directory_handler}
      ]}}
		]}
	]),
  cowboy:start_clear(http, 100, [{port, 8090}], #{
		env => #{dispatch => Dispatch}
	}).

start(_Type, _Args) ->
	clients:start(),
	websocket_sup:start_link().

stop(_State) ->
	ok.
