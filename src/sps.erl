-module(sps).
-author("Ivan Blinkov <ivan@blinkov.ru>").

-export([start/0, stop/0]).
-export([
	subscribe/2, subscribe/3,
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

-spec subscribe(Channels :: binary() | [binary()],
	Conn :: conn()) -> ok.
subscribe(Channels, Conn) ->
	subscribe(Channels, Conn, false).

-spec subscribe(Channels :: binary() | [binary()],
	Conn :: conn(), Deflate :: boolean()) -> ok.
subscribe(Channels, Conn, Deflate) 
	when is_list(Channels) and is_boolean(Deflate) ->
	[subscribe(Channel, Conn, Deflate) || Channel <- Channels],
	ok;

subscribe(Channel, Conn, Deflate)
	when is_boolean(Deflate) ->
	gen_server:cast(
		sockjs_pubsub_util:get_manager_atom(Channel),
		{subscribe, Channel, Conn, Deflate}
	).

-spec unsubscribe(Channels :: binary() | [binary()],
	Conn :: conn()) -> ok.
unsubscribe(Channels, Conn) when is_list(Channels) ->
	[unsubscribe(Channel, Conn) || Channel <- Channels],
	ok;

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
	