#!/usr/bin/env bash
set -u

: "${NODE_NAME:?Need NODE_NAME}"
: "${COOKIE:?Need COOKIE}"
: "${MNESIA_DIR:=/data/mnesia}"

REL=/app/_build/prod/rel/chat_app/bin/chat_app
VMARGS_DIR=/app/_build/prod/rel/chat_app/releases
VMARGS=$(ls -1 ${VMARGS_DIR}/*/vm.args | head -n 1)

echo "Starting node: ${NODE_NAME}"
echo "Peers: ${PEERS:-<none>}"
echo "Mnesia dir: ${MNESIA_DIR}"
echo "vm.args path: ${VMARGS}"

# -------------------------------------------------------------------
# 🔴 CRITICAL FIX #1: ensure Mnesia directory exists
# -------------------------------------------------------------------
mkdir -p "${MNESIA_DIR}"

# Ensure epmd is running (distribution depends on it)
epmd -daemon || true

# -------------------------------------------------------------------
# Overwrite vm.args at runtime (this is the source of truth)
# -------------------------------------------------------------------
cat > "${VMARGS}" <<EOF
-sname ${NODE_NAME}
-setcookie ${COOKIE}

+K true
+A 16
EOF

echo "---- vm.args (effective) ----"
cat "${VMARGS}"
echo "-----------------------------"

# -------------------------------------------------------------------
# Start BEAM in foreground but backgrounded by shell
# -------------------------------------------------------------------
${REL} foreground &
PID=$!

# Give BEAM time to boot
sleep 5

# -------------------------------------------------------------------
# Best-effort connect to peers
# -------------------------------------------------------------------
if [ -n "${PEERS:-}" ]; then
  for p in ${PEERS}; do
    echo "Connecting to ${p}"
    ${REL} rpc net_kernel connect_node "['${p}']" || true
  done
fi

# -------------------------------------------------------------------
# Best-effort Mnesia sync (NEVER fatal)
# -------------------------------------------------------------------
echo "Syncing Mnesia replicas"
${REL} rpc chat_store sync_replicas "[]" || true

# -------------------------------------------------------------------
# Keep container alive
# -------------------------------------------------------------------
wait $PID
