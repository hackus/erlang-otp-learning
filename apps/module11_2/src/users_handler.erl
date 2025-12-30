%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 12:07 PM
%%%-------------------------------------------------------------------
-module(users_handler).
-author("hackus").

%% API
-export([init/2]).

init(Req, State) ->
  Id = cowboy_req:binding(id, Req),

  case users_store:get(Id) of
    {ok, User} ->
      Body = jsx:encode(User),
      Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req),
      {ok, Req2, State};

    not_found ->
      Req2 = cowboy_req:reply(404, Req),
      {ok, Req2, State}
  end.
