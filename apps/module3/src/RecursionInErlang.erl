%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2025 7:56 PM
%%%-------------------------------------------------------------------
-module('RecursionInErlang').
-author("hackus").

%% API
-export([sum/1, badSum/1]).

%% bad recursion works at return type level
badSum([]) ->
  0;
badSum([H|T]) ->
  H + badSum(T).


%% tail recursion, works at parameter level
sum(List) ->
  sum(List, 0).

sum([], Acc) ->
  Acc;

sum([H|T], Acc) ->
  sum(T, H + Acc).

