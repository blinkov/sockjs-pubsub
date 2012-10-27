-module(sps).
-author("Ivan Blinkov <ivan@blinkov.ru>").

-export([start/0, stop/0]).
-export([
	subscribe/2,
	unsubscribe/2,
	publish/2
]).

-type(conn() :: {sockjs_session, any()}).

-spec start() -> ok.
start() ->
	application:start(sockjs_pubsub).

-spec stop() -> ok.
stop() ->
	application:stop(sockjs_pubsub).

-spec subscribe(Channel :: binary(), Conn :: conn()) -> ok.
subscribe(Channel, Conn) ->
	gen_server:cast(
		sockjs_pubsub_util:get_manager_atom(Channel),
		{subscribe, Channel, Conn}
	).

-spec unsubscribe(Channel :: binary(), Conn :: conn()) -> ok.
unsubscribe(Channel, Conn) ->
	gen_server:cast(
		sockjs_pubsub_util:get_manager_atom(Channel),
		{unsubscribe, Channel, Conn}
	).


-spec publish(Channel :: binary(), Message :: any()) -> ok.
publish(Channel, Message) ->
	gen_server:abcast(
		sockjs_pubsub_util:all_nodes(),
		sockjs_pubsub_util:get_manager_atom(Channel),
		{publish, Channel, Message}
	), 
	ok.
	