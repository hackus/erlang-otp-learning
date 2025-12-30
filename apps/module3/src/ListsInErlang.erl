%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2025 3:52 PM
%%%-------------------------------------------------------------------
-module('ListsInErlang').
-author("hackus").

%% API
-export([handle/1, reverse/1, push/2, listMapExample/1, listFlatMapExample/1, applyComprehension/1]).

%% Cons operator
push(Item, Stack) ->
  [Item | Stack].

%% Reverse
reverse(List) ->
  reverse(List, []).

reverse([], Acc) ->
  Acc;

reverse([H | T], Acc) ->
  reverse(T, [H | Acc]).

%% Pattern matching
%% Command format:
%% [add, Value, List]
%% [remove, Value, List]
%% [stop]
handle([add, X, List]) ->
  NewList = [X | List],
  io:format("Added ~p → ~p~n", [X, NewList]),
  {ok, NewList};

handle([remove, X, List]) ->
  NewList = lists:delete(X, List),
  io:format("Deleted ~p → ~p~n", [X, NewList]),
  {ok, NewList};

handle([length, List]) ->
  Len = length(List),
  io:format("List length ~p~n", [Len]),
  {ok, Len};

handle([stop]) ->
  io:format("Stopping command received~n", []),
  {ok, stopped};

handle(_) ->
  {error, unknown_command}.

%% List map
listMapExample(List) ->
  lists:map(fun(X) -> [X, X] end, List).

%% List flatMap
listFlatMapExample(List) ->
  lists:flatmap(fun(X) -> [X, X] end, List).

%% List comprehension
applyComprehension(List) ->
  [Y || X <- List, Y <- [X, X]].