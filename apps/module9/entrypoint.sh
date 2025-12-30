#!/bin/sh

echo ">> Setting Erlang cookie"
echo "$RELEASE_COOKIE" > /root/.erlang.cookie
chmod 400 /root/.erlang.cookie

echo ">> Starting node with distributed limits"
exec /app/_build/default/rel/module9/bin/module9 foreground \
  -kernel inet_dist_listen_min 9100 \
  -kernel inet_dist_listen_max 9105
