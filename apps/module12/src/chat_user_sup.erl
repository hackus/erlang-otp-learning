%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 2:52 PM
%%%-------------------------------------------------------------------
-module(chat_user_sup).
-author("hackus").

-behaviour(supervisor).

-export([start_link/0, start_session/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Arg can be "alice" OR {User, NotifyPid}
start_session(Arg) ->
  supervisor:start_child(?MODULE, [Arg]).

init([]) ->
  Child = #{
    id => chat_user_session,
    start => {chat_user_session, start_link, []},
    restart => temporary,
    shutdown => 5000,
    type => worker,
    modules => [chat_user_session]
  },
  {ok, {{simple_one_for_one, 10, 10}, [Child]}}.


