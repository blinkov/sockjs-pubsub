-module(sockjs_pubsub).
-behavior(application).
-author("Ivan Blinkov <ivan@blinkov.ru>").

-include("constants.hrl").

-export([
	start/2,
	stop/1
]).

start(_Type, _Args) ->
	sockjs_pubsub_sup:start_link().

stop(_State) ->
	ok.