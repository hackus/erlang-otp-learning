%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 12:07 PM
%%%-------------------------------------------------------------------
-module(users_store).
-author("hackus").

%% API
-export([init/0, get/1]).

-define(TABLE, users).

init() ->
  ets:new(?TABLE, [named_table, public, set]),
  ets:insert(?TABLE, {<<"1">>, #{id => 1, name => <<"Alice">>}}),
  ets:insert(?TABLE, {<<"2">>, #{id => 2, name => <<"Bob">>}}),
  ok.

get(Id) ->
  case ets:lookup(?TABLE, Id) of
    [{_, User}] -> {ok, User};
    [] -> not_found
  end.