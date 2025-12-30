%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Dec 2025 8:16 PM
%%%-------------------------------------------------------------------
-module('RunWorker').
-author("hackus").

-behaviour(gen_server).

-export([start_link/0, say_hello/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, 'RunWorker'}, ?MODULE, [], []).

say_hello() ->
  gen_server:call('RunWorker', hello).

init([]) ->
  io:format("Worker started~n"),
  {ok, []}.

handle_call(hello, _From, State) ->
  io:format("Hello from worker~n"),
  {reply, ok, State};
handle_call(_, _, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("Worker terminating~n"),
  ok.

code_change(_Old, State, _Extra) ->
  {ok, State}.
