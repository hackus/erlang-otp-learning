%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2025 10:14 PM
%%%-------------------------------------------------------------------
-module('Concurency_tests').
-author("hackus").

%%%-------------------------------------------------------------------
%%% @doc
%%%   Tests for the Concurency module
%%%-------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% Export tests (EUnit auto-discovers *_test() and *_test_() functions)
%%-export([counter_test/0]).

%%%-------------------------------------------------------------------
%%% Test: sending messages increments the counter correctly
%%%-------------------------------------------------------------------

counter_test() ->
    %% Start worker process
    Pid = 'Concurency':start(),

    %% Send 1st message
    Pid ! {msg, "hello"},
    timer:sleep(50),   %% give process time to handle message

    %% Send 2nd message
    Pid ! {msg, "world"},
    timer:sleep(50),

    %% Ask for count
    Pid ! get_count,
    timer:sleep(50),

    %% Now verify counter using a trick:
    %% The process dictionary inside the shell does NOT change,
    %% so we check the printed output is correct by re-calling getCounter
    ?assertEqual(2, get_test_count(Pid)),

    %% Stop the worker
    Pid ! stop.

%%%-------------------------------------------------------------------
%%% Helper: Ask process for count synchronously
%%%-------------------------------------------------------------------
get_test_count(Pid) ->
    %% Send a synchronous request for the value
    Self = self(),
    Pid ! {get_count_sync, Self},

    receive
        {count, Value} -> Value
    after 200 ->
        timeout
    end.
