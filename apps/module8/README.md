# Start docker with erlang in WSL
# In WSL
docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash

# Compile
rebar3 compile

# Utils
Check app name:
ls _build/default/lib/*/ebin/*.app


# Run
erl -pa _build/default/lib/*/ebin
application:start(module8).
application:get_all_key(module8).
'RunWorker':say_hello().
whereis('RunWorker').
init:stop().

# Compiled structure
Application name = module8
Entry callback module = RunRelease
Worker/supervisor modules = RunWorker, RunSupervisor

# Boot order
application:start(module8)
→ load RunRelease
→ call RunRelease:start/2
→ start supervisor
→ start worker

# Release
1.
rebar3 new release RunRelease_rel
or
add in rebar.config: 

{relx, [
{release, {hello_app, "0.1.0"}, [hello_app]},
{dev_mode, true},
{include_erts, false}
]}.

2.
rebar3 release

rebar3 clean
rm -rf _build

/app/_build/default/lib/module8/ebin/module8.app

_build/default/rel/module8/bin/module8 foreground

_build/default/rel/module8/bin/module8 console
'RunWorker':say_hello().


_build/default/rel/module8/bin/module8 console \
-name node1@127.0.0.1 \
-setcookie choco

_build/default/rel/module8/bin/module8 console \
-name node2@127.0.0.1 \
-setcookie choco

net_adm:ping('node1@127.0.0.1').

# B run on nodes
docker ps -a | grep node
docker rm -f node1 node2 node3
docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash
rebar3 clean
rm -rf _build
rebar3 release // wait for about 20 minutes
docker compose down -v
docker compose up


docker compose down -v
docker build -t module8:latest .
docker build -t module8:0.1.0 .
docker compose up -d --force-recreate
docker exec -it node1 /app/_build/default/rel/module8/bin/module8 remote_console

docker compose down -v
docker compose up --build
docker exec -it node1 /app/_build/default/rel/module8/bin/module8 remote_console
net_adm:ping('node2@fc1c0785c435').
docker exec -it node2 /app/_build/default/rel/module8/bin/module8 remote_console
net_adm:ping('node1@8a1bda986ae5').

# To exit
exit(whereis('<File>'), kill).
or
unregister('<File>').
or
q().
or 
Ctrl + C