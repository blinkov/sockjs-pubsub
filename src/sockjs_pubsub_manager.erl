-module(sockjs_pubsub_manager).
-behaviour(gen_server).
-author("Ivan Blinkov <ivan@blinkov.ru>").

-include("constants.hrl").
-define(NAME, State#state.name).

-export([
	start_link/1
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	name :: atom()
}).

start_link([Name]) ->
	gen_server:start_link({local, Name}, ?MODULE, [Name], []).

init([Name]) ->
	ets:new(Name, [
		named_table, 
		private
	]),
	{ok, #state{name = Name}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({publish, Channel, Message}, State) ->
	{ok, Tid} = get_tid(Channel, State),
	spawn(fun() ->
		EncodedMessage = mochijson2_fork:encode(Message),
		DeflatedMessage = base64:encode(zlib:zip(EncodedMessage)),
		ets:foldl(fun({Conn, Deflate}, _Acc) ->
			{sockjs_session,{Pid, _}} = Conn,
			case is_process_alive(Pid) of
				true ->
					case Deflate of
						true ->
							sockjs:send(DeflatedMessage, Conn);
						false ->
							sockjs:send(EncodedMessage, Conn)
					end;
				false ->
					ets:delete(Tid, Conn)
			end
		end, ok, Tid)
	end),
	{noreply, State};

handle_cast({subscribe, Channel, Conn, Deflate}, State) ->
	{ok, Tid} = get_tid(Channel, State),
	ets:insert(Tid, {Conn, Deflate}),
	{noreply, State};

handle_cast({unsubscribe, Channel, Conn}, State) ->
	{ok, Tid} = get_tid(Channel, State),
	ets:delete(Tid, Conn),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

get_tid(Channel, State) ->
	case ets:lookup(?NAME, Channel) of
		[{_Channel, Tid}] ->
			{ok, Tid};
		[] ->
			NewTid = ets:new(channel, [
				public,
				{read_concurrency, true}
			]),
			ets:insert(?NAME, {Channel, NewTid}),
			{ok, NewTid}
	end.