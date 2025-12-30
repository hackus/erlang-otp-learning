# Start docker with erlang in WSL
# In WSL
docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash

# Compile
[//]: # (erlc ./src/RunSupervisor.erl)
erlc -o . ./src/*.erl

# Run
erl
Scenario 1
'RunSupervisor':start_link().
whereis('WorkerGenServer').
'WorkerGenServer':crash(). // worker crash, restarted
gen_server:call(WorkerGenServer, {boom}). // all crash
Scenario 2
RunSupervisor:start_link().
whereis('WorkerGenServer').
gen_server:cast('WorkerGenServer', boom).
whereis('WorkerGenServer').
catch gen_server:call('WorkerGenServer', {boom}). // worker crash, restarted
whereis('WorkerGenServer').
Scenario 3
application:start(module7).
whereis('RunSupervisor').
whereis('WorkerGenServer').
application:stop(module7).

# To exit
supervisor:terminate_child('RunSupervisor', 'WorkerGenServer').
supervisor:delete_child('RunSupervisor', 'WorkerGenServer').
or
exit(whereis('RunSupervisor'), shutdown).
or
exit(whereis('RunSupervisor'), kill).
or
gen_server:cast('WorkerGenServer', stop).
or
application:stop(module7).
or
unregister('<File>').
or
q().
or
Ctrl + C