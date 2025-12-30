%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2025 3:50 PM
%%%-------------------------------------------------------------------
-module('RunListServer').
-behaviour(gen_server).
-author("hackus").

%% API
-export([start_link/0, add_sync/1, get_sync/0, clear_sync/0,
  add_async/1, clear_async/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% Public API
%%====================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% -------- Synchronous operations --------
add_sync(Item) ->
  gen_server:call(?SERVER, {add, Item}).

clear_sync() ->
  gen_server:call(?SERVER, clear).

get_sync() ->
  gen_server:call(?SERVER, get).

%% -------- Asynchronous operations --------

add_async(Item) ->
  gen_server:cast(?SERVER, {add, Item}).

clear_async() ->
  gen_server:cast(?SERVER, clear).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
  %% Initial empty list
  {ok, []}.

%% -------- handle_call (sync) --------
handle_call({add, Item}, _From, State) ->
  NewState = [Item | State],
  {reply, ok, NewState};

handle_call(get, _From, State) ->
  {reply, lists:reverse(State), State};

handle_call(clear, _From, _State) ->
  {reply, ok, []};

handle_call(_, _From, State) ->
  {reply, error, State}.

%%handle_call({add, Item}, _From, State) when is_integer(Item) ->
%%  {reply, ok, [Item | State]};
%%
%%handle_call({add, Bad}, _From, State) ->
%%  {reply, {error, bad_item}, State};

%% -------- handle_cast (async) --------
handle_cast({add, Item}, State) ->
  {noreply, [Item | State]};

handle_cast(clear, _State) ->
  {noreply, []};

%%handle_cast({add, Item}, State) when is_integer(Item) ->
%%  {noreply, [Item | State]};
%%
%%handle_cast({add, _Bad}, State) ->
%%  {noreply, State}.

handle_cast(_, State) ->
  {noreply, State}.

%% -------- other callbacks --------
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
