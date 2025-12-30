%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 2:53 PM
%%%-------------------------------------------------------------------
-module(chat_store).
-author("hackus").
-behaviour(gen_server).

%% API
-export([sync_replicas/0]).
-export([start_link/0]).
-export([register/2, auth/2, store_offline/2, take_offline/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(user, {username, passhash}).
-record(offline_msg, {to_user, msg}).  % bag table

%% -------------------------------------------------------------------
%% Public
%% -------------------------------------------------------------------

sync_replicas() ->
  %% Ensure tables exist AND follower has local copies
  ensure_tables().

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(U, P) -> gen_server:call(?SERVER, {register, U, P}).
auth(U, P)     -> gen_server:call(?SERVER, {auth, U, P}).
store_offline(ToUser, Msg) -> gen_server:call(?SERVER, {store_offline, ToUser, Msg}).
take_offline(ToUser) -> gen_server:call(?SERVER, {take_offline, ToUser}).

%% -------------------------------------------------------------------
%% gen_server
%% -------------------------------------------------------------------

init([]) ->
  ok = ensure_mnesia(),
  self() ! init_tables,
  {ok, #{}}.

handle_call({register, U, P}, _From, St) ->
  PassHash = hash(P),
  F = fun() ->
    case mnesia:read({user, U}) of
      [] ->
        mnesia:write(#user{username=U, passhash=PassHash}),
        ok;
      [_] ->
        {error, exists}
    end
      end,
  {reply, tx(F), St};

handle_call({auth, U, P}, _From, St) ->
  PassHash = hash(P),
  F = fun() ->
    case mnesia:read({user, U}) of
      [#user{passhash=PassHash}] -> ok;
      _ -> {error, invalid}
    end
      end,
  {reply, tx(F), St};

handle_call({store_offline, ToUser, Msg}, _From, St) ->
  {reply, tx(fun() ->
    mnesia:write(#offline_msg{to_user=ToUser, msg=Msg}),
    ok
             end), St};

handle_call({take_offline, ToUser}, _From, St) ->
  {reply, tx(fun() ->
    Recs = mnesia:read({offline_msg, ToUser}),
    Msgs = [R#offline_msg.msg || R <- Recs],
    lists:foreach(fun(R) -> mnesia:delete_object(R) end, Recs),
    Msgs
             end), St}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(init_tables, State) ->
  case ensure_tables() of
    ok ->
      io:format("[chat_store] tables ready on ~p~n", [node()]),
      {noreply, State};

    {error, timeout} ->
      io:format("[chat_store] tables not ready yet, retrying~n"),
      erlang:send_after(1000, self(), init_tables),
      {noreply, State};

    {error, Reason} ->
      io:format("[chat_store] ensure_tables error: ~p~n", [Reason]),
      erlang:send_after(1000, self(), init_tables),
      {noreply, State}
  end;

handle_info(_, State) ->
  {noreply, State}.

terminate(_, _) ->
  ok.

%% -------------------------------------------------------------------
%% Helpers
%% -------------------------------------------------------------------

ensure_mnesia() ->
  Dir = os:getenv("MNESIA_DIR", "Mnesia"),
  application:set_env(mnesia, dir, Dir),

  case os:getenv("MNESIA_MASTER") of
    "true" ->
      _ = mnesia:create_schema([node()]);
    _ ->
      ok
  end,

  ok = application:start(mnesia),
  ok = mnesia:wait_for_tables([schema], 30000),
  ok.

ensure_tables() ->
  Tabs = [user, offline_msg],
  case os:getenv("MNESIA_MASTER") of
    "true" ->
      create_table(user, record_info(fields, user), set),
      create_table(offline_msg, record_info(fields, offline_msg), bag),
      wait_for_tables(Tabs);

    _ ->
      %% follower
      ok = join_mnesia_cluster(),
      case wait_for_tables(Tabs) of
        ok ->
          %% optional: local persistence on follower
          _ = add_table_copy(user),
          _ = add_table_copy(offline_msg),
          ok;
        Error ->
          Error
      end
  end.


wait_for_tables(Tabs) ->
  case mnesia:wait_for_tables(Tabs, 30000) of
    ok -> ok;
    {timeout, _} -> {error, timeout};
    Other -> {error, Other}
  end.

add_table_copy(Tab) ->
  case mnesia:add_table_copy(Tab, node(), disc_copies) of
    {atomic, ok} ->
      ok;
    {aborted, {already_exists, _, _}} ->
      ok;
    {aborted, {already_exists, _}} ->
      ok;
    {aborted, {no_exists, _}} ->
      {error, not_ready};
    Other ->
      {error, Other}
  end.

create_table(Tab, Attrs, Type) ->
  case mnesia:create_table(Tab, [
    {attributes, Attrs},
    {type, Type},
    {disc_copies, [node()]}
  ]) of
    {atomic, ok} -> ok;
    {aborted, {already_exists, _}} -> ok;
    Other -> Other
  end.

join_mnesia_cluster() ->
  %% master/schema node (set via docker-compose)
  SchemaNode =
    list_to_atom(
      os:getenv("MNESIA_SCHEMA_NODE", "chat1@node1")
    ),

  %% Join the Mnesia cluster (best-effort)
  _ = mnesia:change_config(extra_db_nodes, [SchemaNode]),
  ok.

tx(Fun) ->
  case mnesia:transaction(Fun) of
    {atomic, Reply} -> Reply;
    {aborted, Reason} -> {error, Reason}
  end.

hash(P) when is_list(P)   -> crypto:hash(sha256, P);
hash(P) when is_binary(P) -> crypto:hash(sha256, P).
