%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Dec 2025 6:44 PM
%%%-------------------------------------------------------------------
-module(chat_app).
-author("hackus").

-behaviour(application).

%%%-------------------------------------------------------------------
%%% @doc Distributed auto-connecting chat app
%%%-------------------------------------------------------------------

-export([start/2, stop/1]).

start(_Type, _Args) ->
  Pid = spawn(fun loop/0),
  spawn(fun init_node/0),
  {ok, Pid}.

loop() ->
  receive
    stop -> ok;
    _ -> loop()
  end.

init_node() ->
  timer:sleep(1500),

  %% Get our node name (example: 'node1@node1')
  ThisNode = node(),

  %% Extract base name (node1 or node2)
  ThisName = list_to_atom(hd(string:tokens(atom_to_list(ThisNode), "@"))),

  %% Start chat user named after node
  chat_user:start(ThisName),

  %% Known nodes in system
  AllNodes = ['node1@node1.local', 'node2@node2.local'],

  %% Ping all except ourselves
  OtherNodes = lists:delete(ThisNode, AllNodes),
  lists:foreach(
    fun(N) ->
      case net_adm:ping(N) of
        pong -> io:format("Connected to ~p~n", [N]);
        _ -> io:format("No response from ~p~n", [N])
      end
    end,
    OtherNodes
  ),

  ok.

stop(_State) ->
  ok.

