%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 2:52 PM
%%%-------------------------------------------------------------------
-module(chat_user_session).
-author("hackus").
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {username, notify_pid}).

start_link({Username, NotifyPid}) ->
  gen_server:start_link(?MODULE, {Username, NotifyPid}, []);
start_link(Username) ->
  %% backward compatible
  gen_server:start_link(?MODULE, {Username, undefined}, []).

init({Username, NotifyPid0}) ->
  %% if NotifyPid not provided, default to group leader (still no io here)
  NotifyPid =
    case NotifyPid0 of
      undefined -> undefined;
      Pid when is_pid(Pid) -> Pid
    end,

  %% offline replay (also goes to mailbox)
  Msgs = chat_store:take_offline(Username),
  lists:foreach(fun(M) -> self() ! {deliver, M} end, Msgs),

  {ok, #state{username = Username, notify_pid = NotifyPid}}.

handle_call({send, ToUser, Text}, _From, State = #state{username = From}) ->
  case chat_registry:whereis(ToUser) of
    {ok, ToPid} ->
      ToPid ! {deliver, #{from => From, text => Text}},
      {reply, ok, State};
    not_found ->
      chat_store:store_offline(ToUser, #{from => From, text => Text}),
      {reply, offline, State}
  end;

handle_call(_, _From, State) ->
  {reply, error, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info({deliver, Msg}, State = #state{notify_pid = NotifyPid}) ->
  %% Send to the login shell process mailbox
  case NotifyPid of
    Pid when is_pid(Pid) ->
      Pid ! {chat_message, Msg};
    _ ->
      ok
  end,
  {noreply, State};

handle_info(_, State) ->
  {noreply, State}.

terminate(_, _) ->
  ok.
