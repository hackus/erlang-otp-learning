# Start docker with erlang in WSL
# In WSL
docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash

# Compile
erlc ./src/<File.erl>

docker compose up --build
rm -rf _build
rm -f rebar.lock

docker compose build --no-cache
docker compose up

docker compose down --remove-orphans

docker compose build --no-cache

docker rm -f module11_2

# Run
docker exec -it module11_1 curl -v http://127.0.0.1:8080/health
curl http://localhost:8080/health

curl -X POST http://localhost:8080/echo \
-H "Content-Type: application/json" \
-d '{"hello":"world"}'

curl http://localhost:8080/users/1

# To exit
exit(whereis('<File>'), kill).
or
unregister('<File>').
or
q().
or 
Ctrl + C