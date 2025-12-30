# Development Environment (Live Reload Style)

Runs your Erlang project with volumes mounted from the host.

Benefits:
- No rebuild needed after editing .erl files
- Tests run instantly
- Ideal for development and homework

Usage:

cd docker/dev
docker-compose up --build

run this inside wsl:
cd /mnt/d/projects/erlang/LearnErlang
docker compose -f docker/dev/docker-compose.yml up --build