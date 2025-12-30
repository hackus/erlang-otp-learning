%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 12:06 PM
%%%-------------------------------------------------------------------
-module(echo_handler).
-author("hackus").

%% API
-export([init/2]).

init(Req, State) ->
  {ok, BodyBin, Req1} = cowboy_req:read_body(Req),
  Data = jsx:decode(BodyBin, [return_maps]),

  Resp = jsx:encode(#{
    echo => Data
  }),

  Req2 = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"application/json">>},
    Resp,
    Req1
  ),

  {ok, Req2, State}.
