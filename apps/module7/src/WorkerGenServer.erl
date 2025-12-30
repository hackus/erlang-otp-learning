%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Nov 2025 11:35 PM
%%%-------------------------------------------------------------------
-module('WorkerGenServer').
-author("hackus").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).
-export([fullCrash/0, fullCrash2/0, crash/0]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  io:format("Worker started~n"),
  {ok, #{}}.

handle_call({boom}, _From, State) ->
  1 = 2,  %% forced crash (badmatch)
  {reply, ok, State};

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast(boom, State) ->
  io:format("Manual crash triggered~n"),
  erlang:error(manual_crash),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({explode}, _State) ->
  erlang:error(bad_info_message);

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

fullCrash() ->
  erlang:error(simulated_crash).

crash() ->
  gen_server:cast(?MODULE, boom).

fullCrash2() ->
  exit(simulated_exit).