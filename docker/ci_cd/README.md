# CI/CD Docker Image

This Dockerfile builds a complete image containing:

- Erlang/OTP
- rebar3
- src/
- test/
- rebar.config

It runs EUnit tests automatically via CMD.

Usage:

docker build -t erlang-homework -f docker/ci_cd/Dockerfile .
docker run --rm erlang-homework
