%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2025 8:32 PM
%%%-------------------------------------------------------------------
-module(ets_counter_store).
-author("hackus").
-behaviour(gen_server).

%% Public API
-export([
  start_link/0,
  incr/1,
  get/1,
  reset/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {table}).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Increment counter for Key and return new value
incr(Key) ->
  gen_server:call(?MODULE, {incr, Key}).

%% Read current value (or 0 if missing)
get(Key) ->
  gen_server:call(?MODULE, {get, Key}).

%% Reset counter to 0
reset(Key) ->
  gen_server:call(?MODULE, {reset, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  %% Simple set-table, public so you *could* inspect with ets:tab2list/1
  Table = ets:new(?MODULE, [
    set,
    public,
    named_table
  ]),
  {ok, #state{table = Table}}.

handle_call({incr, Key}, _From, S = #state{table = T}) ->
  %% position 2 in tuple {Key, Value}
  NewVal = ets:update_counter(T, Key, {2, 1}, {Key, 0}),
  {reply, NewVal, S};

handle_call({get, Key}, _From, S = #state{table = T}) ->
  Reply =
    case ets:lookup(T, Key) of
      [{_, V}] -> V;
      [] -> 0
    end,
  {reply, Reply, S};

handle_call({reset, Key}, _From, S = #state{table = T}) ->
  ets:insert(T, {Key, 0}),
  {reply, ok, S};

handle_call(_Other, _From, State) ->
  {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
