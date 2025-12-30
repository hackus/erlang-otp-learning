%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 2:53 PM
%%%-------------------------------------------------------------------
-module(chat_registry).
-author("hackus").
-behaviour(gen_server).

-export([start_link/0, login/2, logout/1, list_users/0, whereis/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(User, Pass) ->
  gen_server:call(?MODULE, {login, User, Pass}).

logout(Pid) ->
  gen_server:call(?MODULE, {logout, Pid}).

list_users() ->
  gen_server:call(?MODULE, list_users).

whereis(User) ->
  gen_server:call(?MODULE, {whereis, User}).

init([]) ->
  %% Explicitly start pg if not running (OTP 26/27 + releases)
  case erlang:whereis(pg) of
    undefined -> {ok, _} = pg:start_link();
    _ -> ok
  end,
  {ok, #{}}.

%% NOTE: capture caller pid from From = {CallerPid, Tag}
handle_call({login, User, Pass}, From, State) ->
  CallerPid = element(1, From),  %% From = {Pid, Tag}
  case chat_store:auth(User, Pass) of
    ok ->
      {ok, Pid} = chat_user_sup:start_session({User, CallerPid}),
      Ref = erlang:monitor(process, Pid),

      pg:join(all_users, Pid),
      pg:join({user, User}, Pid),

      {reply, {ok, Pid}, State#{Pid => {User, Ref}}};

    Error ->
      {reply, Error, State}
  end;

handle_call({logout, Pid}, _From, State) ->
  case maps:take(Pid, State) of
    {{User, Ref}, NewState} ->
      erlang:demonitor(Ref, [flush]),
      pg:leave(all_users, Pid),
      pg:leave({user, User}, Pid),
      exit(Pid, normal),
      {reply, ok, NewState};

    error ->
      {reply, {error, not_logged_in}, State}
  end;

handle_call(list_users, _From, State) ->
  Groups = pg:which_groups(),
  Users0 = [Username || {user, Username} <- Groups],
  Users  = [U || U <- Users0, pg:get_members({user, U}) =/= []],
  {reply, lists:usort(Users), State};

handle_call({whereis, User}, _From, State) ->
  case pg:get_members({user, User}) of
    [Pid | _] -> {reply, {ok, Pid}, State};
    []        -> {reply, not_found, State}
  end;

handle_call(Other, _From, State) ->
  {reply, {error, {unknown_call, Other}}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
  case maps:take(Pid, State) of
    {{User, Ref}, NewState} ->
      %% Ensure we leave global presence even on crash
      pg:leave(all_users, Pid),
      pg:leave({user, User}, Pid),
      %% optional: keep a log line if you want (remove if you hate logs)
      io:format("[registry] ~p (~p) down: ~p~n", [User, Pid, Reason]),
      {noreply, NewState};

    error ->
      {noreply, State}
  end;

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
