%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 2:51 PM
%%%-------------------------------------------------------------------
-module(chat_sup).
-author("hackus").

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  % pg is built-in; no supervisor needed, but it’s safer to ensure it's started (kernel starts it in modern OTP)
  % We start:
  % - chat_store (mnesia init)
  % - chat_registry (presence + login)
  % - chat_user_sup (dynamic sessions)
  Children = [
    {chat_store,
      {chat_store, start_link, []},
      permanent, 5000, worker, [chat_store]},

    {chat_registry,
      {chat_registry, start_link, []},
      permanent, 5000, worker, [chat_registry]},

    {chat_user_sup,
      {chat_user_sup, start_link, []},
      permanent, 5000, supervisor, [chat_user_sup]}
  ],
  {ok, {{one_for_one, 5, 10}, Children}}.

