#!/bin/bash
set -e

DOCKER_DISTRO="Ubuntu-22.04"

MODE="$1"

info() { echo -e "🔹 $1"; }

echo current path
pwd
#cd ..
#cd ..
#pwd
#cd apps/module2/
#pwd

#cd ..
#cd dev

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
PROJECT_ROOT="$(realpath "$SCRIPT_DIR/../.." )"
DEV_DIR="$PROJECT_ROOT/docker/dev"

echo DEV_DIR "$DEV_DIR"


### CI mode
if [ "$MODE" = "--cicd" ]; then
    info "Running CI/CD tests..."

    echo current directory:
    pwd
    cd "$PROJECT_ROOT"
    docker build -t erlang-homework-ci -f docker/ci_cd/Dockerfile .
    docker run --rm erlang-homework-ci

    exit $?
fi

### DEV mode
if [ "$MODE" = "--dev" ]; then
    info "Running DEV tests (live code)..."

    cd "$DEV_DIR"
    docker compose down --remove-orphans >/dev/null 2>&1 || true
    docker compose up --build --abort-on-container-exit --exit-code-from erlang
    docker compose down --remove-orphans

    exit $?
fi
