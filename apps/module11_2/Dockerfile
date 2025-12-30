#######################################
# Stage 1 — Build OTP release
#######################################
FROM erlang:27 AS builder

WORKDIR /app
COPY . .

ENV RELX_REPLACE_OS_VARS=true
RUN rebar3 release

#######################################
# Stage 2 — Runtime
#######################################
FROM debian:bookworm-slim

# Install runtime dependencies for Erlang/OTP
RUN apt-get update && apt-get install -y \
    libssl3 \
    ca-certificates \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy ONLY the release
COPY --from=builder /app/_build/default/rel/module11_2 ./

EXPOSE 8080

ENTRYPOINT ["bin/module11_2"]
CMD ["foreground"]
