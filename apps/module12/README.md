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

1.
chat_user:register("alice", "pass"), {ok, P1} = chat_registry:login("alice", "pass").
or 
chat_user:register("alice", "pass").
{ok, P1} = chat_registry:login("alice", "pass").
2.
chat_user:send(P1, "bob", "hello bob").

docker exec -it chat12-node2 /app/_build/prod/rel/chat_app/bin/chat_app remote_console
mnesia:system_info(tables).
mnesia:table_info(user, attributes).
mnesia:table_info(offline_msg, type).

1.
chat_user:register("bob", "pass"), {ok, P2} = chat_registry:login("bob", "pass").
or
chat_user:register("bob", "pass").
{ok, P2} = chat_registry:login("bob", "pass").
2.
flush().

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