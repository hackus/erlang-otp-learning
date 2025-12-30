%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2025 8:32 PM
%%%-------------------------------------------------------------------
-module(ets_safe_store).
-author("hackus").
-behaviour(gen_server).

%% API
-export([start_link/0, put/2, get/1, delete/1]).

%% Callbacks
-export([init/1, handle_call/3]).

-record(state, {table}).

%%% =======================
%%% Public API
%%% =======================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Key, Value) ->
  gen_server:call(?MODULE, {put, Key, Value}).

get(Key) ->
  gen_server:call(?MODULE, {get, Key}).

delete(Key) ->
  gen_server:call(?MODULE, {delete, Key}).

%%% =======================
%%% Callbacks
%%% =======================

init([]) ->
  Table = ets:new(?MODULE, [set, protected, named_table]),
  {ok, #state{table = Table}}.

handle_call({put, Key, Value}, _From, State = #state{table = T}) ->
  ets:insert(T, {Key, Value}),
  {reply, ok, State};

handle_call({get, Key}, _From, State = #state{table = T}) ->
  Reply = case ets:lookup(T, Key) of
            [{_, V}] -> {ok, V};
            [] -> not_found
          end,
  {reply, Reply, State};

handle_call({delete, Key}, _From, State = #state{table = T}) ->
  ets:delete(T, Key),
  {reply, ok, State}.
