%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 2:55 PM
%%%-------------------------------------------------------------------
-module(chat_room).
-author("hackus").

%% API
-export([join/2, broadcast/3]).

join(Room, SessionPid) ->
  pg:join({room, Room}, SessionPid),
  ok.

broadcast(Room, FromUser, Text) ->
  Msg = #{room => Room, from => FromUser, ts => erlang:system_time(second), text => Text},
  Members = pg:get_members({room, Room}),
  lists:foreach(fun(Pid) -> Pid ! {deliver, Msg} end, Members),
  ok.
