%%%-------------------------------------------------------------------
%%% Advanced ETS counter store:
%%%  - GenServer wrapper
%%%  - Atomic incr/1 using update_counter
%%%  - bulk_put/1 for batch insert
%%%  - select/1 using match_spec on the ETS table
%%%-------------------------------------------------------------------
-module(ets_advanced_counter_store).
-author("hackus").

-behaviour(gen_server).

%% Public API
-export([
  start_link/0,
  incr/1,
  select/1,
  bulk_put/1
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
  %% Register the GenServer under the module name
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Increment counter for Key and return new value
incr(Key) ->
  gen_server:call(?MODULE, {incr, Key}).

%% Run a select over the ETS table using a match_spec
select(MatchSpec) ->
  ets:select(?MODULE, MatchSpec).

%% Insert a list of {Key,Value} pairs
bulk_put(List) ->
  gen_server:call(?MODULE, {bulk_put, List}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  %% Public named table so we can use ets:select/2 directly by name
  Table = ets:new(?MODULE, [
    set,
    public,
    named_table,
    {read_concurrency, true},
    {write_concurrency, true}
  ]),
  {ok, #state{table = Table}}.

handle_call({incr, Key}, _From, State = #state{table = T}) ->
  %% Position 2: {Key, Value}
  NewVal = ets:update_counter(T, Key, {2, 1}, {Key, 0}),
  {reply, NewVal, State};

handle_call({bulk_put, List}, _From, State = #state{table = T}) ->
  ets:insert(T, List),
  {reply, ok, State};

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
