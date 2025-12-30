%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2025 8:32 PM
%%%-------------------------------------------------------------------
-module(ets_fast_store).
-author("hackus").
-behaviour(gen_server).

%% API
-export([start_link/0, put/2, get/1, delete/1]).
-export([init/1, handle_call/3]).

-record(state, {table}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Public API
put(Key, Value) ->
  gen_server:call(?MODULE, {put, Key, Value}).

delete(Key) ->
  gen_server:call(?MODULE, {delete, Key}).

%% optimized read
get(Key) ->
  case ets:lookup(?MODULE, Key) of
    [{_, V}] -> {ok, V};
    [] -> not_found
  end.

init([]) ->
  Table = ets:new(?MODULE, [
    set,
    public,
    named_table,
    {read_concurrency, true},
    {write_concurrency, true}
  ]),
  {ok, #state{table = Table}}.

handle_call({put, Key, Value}, _From, S = #state{table=T}) ->
  ets:insert(T, {Key, Value}),
  {reply, ok, S};

handle_call({delete, Key}, _From, S = #state{table=T}) ->
  ets:delete(T, Key),
  {reply, ok, S}.
