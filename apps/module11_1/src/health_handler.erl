%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 12:05 PM
%%%-------------------------------------------------------------------
-module(health_handler).
-author("hackus").

%% API
-export([init/2]).

init(Req, State) ->
  Body = jsx:encode(#{status => <<"ok">>}),

  Req2 = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"application/json">>},
    Body,
    Req
  ),

  {ok, Req2, State}.