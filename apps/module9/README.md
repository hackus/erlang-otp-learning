# Start docker with erlang in WSL
# In WSL
docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash

# Compile
erlc ./src/<File.erl>

# Run
docker rm -f node1 node2
docker compose down -v
docker compose down --volumes --remove-orphans
docker build -t module9:0.1.0 .
docker compose up -d --force-recreate

rm -rf _build
docker compose build --no-cache
docker compose up

working
docker compose down --remove-orphans
echo choco > erlang.cookie
chmod 400 erlang.cookie
docker compose up --build

# Verification
docker logs node1
docker run -it --rm module9-node1 sh
cat /app/module9/releases/0.1.0/vm.args


# To exit
exit(whereis('<File>'), kill).
or
unregister('<File>').
or
q().
or 
Ctrl + C