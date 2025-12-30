%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2025 1:53 PM
%%%-------------------------------------------------------------------
-module('RunSupervisor7_tests').
-author("hackus").

-include_lib("eunit/include/eunit.hrl").

%% Helper: get worker pid safely
get_worker_pid() ->
  whereis('WorkerGenServer').

get_supervisor_pid() ->
  whereis('RunSupervisor7').

%% ------------------------------------------------------------------
%% 1. Test application starts and processes are alive
%% ------------------------------------------------------------------
app_start_test() ->
  io:format("Sup whereis RunSupervisor7=~p~n", [whereis('RunSupervisor7')]),
  io:format("Sup whereis RunSupervisor=~p~n", [whereis('RunSupervisor')]),
  io:format("Worker whereis WorkerGenServer=~p~n", [whereis('WorkerGenServer')]),
  io:format("Apps=~p~n", [application:which_applications()]),

  application:stop(module7),   %% safe even if not started
  ok = application:start(module7),
  timer:sleep(100),

  ?assert(is_pid(get_supervisor_pid())),
  ?assert(is_pid(get_worker_pid())).


%% ------------------------------------------------------------------
%% 2. Basic crash via cast (handle_cast)
%% ------------------------------------------------------------------
cast_crash_test() ->
  Worker1 = get_worker_pid(),
  ?assert(is_pid(Worker1)),

  %% Crash worker via cast (safe)
  ok = gen_server:cast('WorkerGenServer', boom),
  timer:sleep(100),

  Worker2 = get_worker_pid(),
  ?assert(is_pid(Worker2)),
  ?assertNotEqual(Worker1, Worker2),  %% restarted

  %% Supervisor must stay alive
  ?assert(is_pid(get_supervisor_pid())).

%% ------------------------------------------------------------------
%% 3. Crash via call (handle_call badmatch)
%% ------------------------------------------------------------------
call_crash_test() ->
  Worker1 = get_worker_pid(),
  ?assert(is_pid(Worker1)),

  %% Use catch so the shell/test process does NOT die
  _ = (catch gen_server:call('WorkerGenServer', {boom})),
  timer:sleep(100),

  Worker2 = get_worker_pid(),
  ?assert(is_pid(Worker2)),
  ?assertNotEqual(Worker1, Worker2),

  %% Supervisor MUST still be alive
  ?assert(is_pid(get_supervisor_pid())).

%% ------------------------------------------------------------------
%% 4. fullCrash() – caller crashes, worker stays alive, PID stays same
%% ------------------------------------------------------------------
fullCrash_test() ->
  Worker1 = get_worker_pid(),
  Sup1 = get_supervisor_pid(),

  ?assert(is_pid(Worker1)),
  ?assert(is_pid(Sup1)),

  %% Trigger caller crash (caught)
  CrashResult = (catch 'WorkerGenServer':fullCrash()),

  %% Verify caller indeed crashed
  ?assertMatch({'EXIT', _}, CrashResult),

  %% Allow time (not needed, but safe)
  timer:sleep(50),

  %% Worker must still be alive with SAME pid
  ?assertEqual(Worker1, get_worker_pid()),

  %% Supervisor must still be alive with SAME pid
  ?assertEqual(Sup1, get_supervisor_pid()).


%% ------------------------------------------------------------------
%% 5. fullCrash2() – same behavior as fullCrash
%% ------------------------------------------------------------------
fullCrash2_test() ->
  Worker1 = get_worker_pid(),
  Sup1 = get_supervisor_pid(),

  ?assert(is_pid(Worker1)),
  ?assert(is_pid(Sup1)),

  %% Trigger caller crash (caught)
  CrashResult = (catch 'WorkerGenServer':fullCrash2()),

  %% Verify caller indeed crashed
  ?assertMatch({'EXIT', _}, CrashResult),

  timer:sleep(50),

  %% Worker must still be running (same pid)
  ?assertEqual(Worker1, get_worker_pid()),

  %% Supervisor must still be running (same pid)
  ?assertEqual(Sup1, get_supervisor_pid()).



%% ------------------------------------------------------------------
%% 6. Stop application cleanly
%% ------------------------------------------------------------------
app_stop_test() ->
  ok = application:stop(module7),
  timer:sleep(50),

  ?assertEqual(undefined, get_worker_pid()),
  ?assertEqual(undefined, get_supervisor_pid()).
