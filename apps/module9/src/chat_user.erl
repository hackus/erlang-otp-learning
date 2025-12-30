%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Dec 2025 6:12 PM
%%%-------------------------------------------------------------------
-module(chat_user).
-author("hackus").

-export([start/1, send/2, loop/1]).

start(Name) ->
  Pid = spawn(?MODULE, loop, [Name]),
  register(Name, Pid),
  {ok, Pid}.

send({User, Node}, Msg) ->
  {User, Node} ! {self(), Msg};
send(User, Msg) ->
  case whereis(User) of
    undefined ->
      io:format("User ~p not local.~n", [User]);
    Pid ->
      Pid ! {self(), Msg}
  end.

loop(Name) ->
  receive
    {From, Msg} ->
      io:format("~p (~p) got message from ~p: ~p~n",
        [Name, node(), From, Msg]),
      loop(Name)
  end.