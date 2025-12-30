%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 2:52 PM
%%%-------------------------------------------------------------------
-module(chat_user_session).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {username}).

%%start_link(Username) ->
%%  gen_server:start_link(?MODULE, Username, []).

start_link(Username) ->
  gen_server:start_link(
    ?MODULE,
    {Username, group_leader()},
    []
  ).

%%init(Username) ->
%%  %% MUST return {ok, State}
%%  State = #state{username = Username},
%%
%%  %% deliver offline messages AFTER init
%%  Msgs = chat_store:take_offline(Username),
%%  lists:foreach(fun(M) -> self() ! {deliver, M} end, Msgs),
%%
%%  {ok, State}.

init({Username, GL}) ->
  group_leader(GL, self()),
  State = #state{username = Username},

  Msgs = chat_store:take_offline(Username),
  lists:foreach(fun(M) -> self() ! {deliver, M} end, Msgs),

  {ok, State}.

handle_call({join_room, Room}, _From, State) ->
  chat_room:join(Room, self()),
  {reply, ok, State};

handle_call({say_room, Room, Text}, _From, State = #state{username = From}) ->
  chat_room:broadcast(
    Room,
    #{
      from      => From,
      from_pid  => self(),
      text      => Text
    }
  ),
  {reply, ok, State};

handle_call(logout, _From, State) ->
  {stop, normal, ok, State};

handle_call({send, ToUser, Text}, _From,
    State = #state{username = From}) ->
  Msg = #{
    from     => From,
    from_pid => self(),
    ts       => erlang:system_time(second),
    text     => Text
  },

  case chat_registry:whereis(ToUser) of
    {ok, ToPid} ->
      ToPid ! {deliver, Msg},
      {reply, ok, State};

    not_found ->
      chat_store:store_offline(ToUser, Msg),
      {reply, offline, State}
  end;

handle_call(_, _From, State) ->
  {reply, error, State}.

handle_cast(_, State) ->
  {noreply, State}.

%%handle_info({deliver, Msg}, State = #state{username = U}) ->
%%%%  io:format("~n[~p] delivered to ~p: ~p~n", [node(), U, Msg]),
%%  logger:info("~p delivered to ~p: ~p", [node(), U, Msg]),
%%%%  logger:info("Delivered to ~p: ~p", [U, Msg]),
%%  {noreply, State};

%%handle_info({deliver, Msg}, State = #state{username = U}) ->
%%  FromUser = maps:get(from, Msg, unknown),
%%  FromNode = maps:get(from_node, Msg, unknown),
%%  logger:info("~p delivered to ~p@~p: ~p@~p says ~p",
%%    [FromNode, U, node(), FromUser, FromNode, maps:get(text, Msg, Msg)]),
%%  {noreply, State};

handle_info({deliver, Msg}, State = #state{username = U}) ->
  FromUser = maps:get(from, Msg, unknown),
  FromPid  = maps:get(from_pid, Msg, undefined),
  FromNode =
    case FromPid of
      undefined -> unknown;
      _ -> node(FromPid)
    end,

  logger:info(
    "~p delivered to ~p@~p: ~p@~p says ~p",
    [
      FromNode,
      U,
      node(),
      FromUser,
      FromNode,
      maps:get(text, Msg, Msg)
    ]
  ),
  {noreply, State};


handle_info(_, State) ->
  {noreply, State}.

terminate(_, _) ->
  ok.
