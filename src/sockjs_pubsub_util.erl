-module(sockjs_pubsub_util).
-author("Ivan Blinkov <ivan@blinkov.ru>").

-include("constants.hrl").

-export([
	all_nodes/0,
	get_manager_atom/1
]).

-spec all_nodes() -> [pid()].
all_nodes() ->
	[node() | nodes()].

-spec get_manager_atom(Channel :: binary()) -> atom().
get_manager_atom(Channel) ->
	Names = ?MANAGER_NAMES,
	Z = zlib:open(),
	CRC = zlib:crc32(Z, Channel),
	zlib:close(Z),
	lists:nth((CRC rem ?MANAGER_NAMES_LENGTH) + 1, Names).

	