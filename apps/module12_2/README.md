# Start docker with erlang in WSL
# In WSL
docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash

# Compile
docker compose down -v
docker compose up --build


docker exec -it chat12-node1 /bin/bash
/app/_build/prod/rel/chat_app/bin/chat_app remote_console

node().
nodes().

docker exec -it chat12-node1 /app/_build/prod/rel/chat_app/bin/chat_app remote_console
mnesia:system_info(tables).
mnesia:table_info(user, attributes).
mnesia:table_info(offline_msg, type).

chat_user:register("alice", "pass").
{ok, P1} = chat_registry:login("alice", "pass").
chat_user:send(P1, "bob", "hello bob").

docker exec -it chat12-node2 /app/_build/prod/rel/chat_app/bin/chat_app remote_console
mnesia:system_info(tables).
mnesia:table_info(user, attributes).
mnesia:table_info(offline_msg, type).

chat_user:register("bob", "pass").
{ok, P2} = chat_registry:login("bob", "pass").

# Room usage example
docker rm -f chat12-node1 chat12-node2
docker compose down --remove-orphans
docker compose rm -f
docker compose up --build

docker exec -it chat12-node1 /app/_build/prod/rel/chat_app/bin/chat_app remote_console
chat_user:register("alice", "pass").
{ok, A} = chat_user:login("alice", "pass").
chat_user:join_room(A, lobby).
chat_user:say_room(A, lobby, "Hello Bob!").

docker exec -it chat12-node2 /app/_build/prod/rel/chat_app/bin/chat_app remote_console
chat_user:register("bob", "pass").
{ok, B} = chat_user:login("bob", "pass").
chat_user:join_room(B, lobby).
group_leader(self(), B).
flush().

Either call this: logger:set_primary_config(level, info). 


# Run
erl

# To exit
exit(whereis('<File>'), kill).
or
unregister('<File>').
or
q().
or 
Ctrl + C