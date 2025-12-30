%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 2:53 PM
%%%-------------------------------------------------------------------
-module(chat_user).
-author("hackus").

%% API
-export([register/2, login/2, send/3, list_users/0, join_room/2, say_room/3, logout/1]).

register(U, P) -> chat_store:register(U, P).
login(U, P)    -> chat_registry:login(U, P).
send(S, To, T) -> gen_server:call(S, {send, To, T}).
list_users()   -> chat_registry:list_users().
join_room(S,R) -> gen_server:call(S, {join_room, R}).
say_room(S,R,T)-> gen_server:call(S, {say_room, R, T}).
logout(S)      -> gen_server:call(S, logout).
