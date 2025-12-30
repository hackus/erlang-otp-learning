#FROM erlang:27 AS builder
#WORKDIR /app
#COPY . .
#ENV RELX_REPLACE_OS_VARS=true
#RUN rebar3 release
#
#FROM erlang:27
#
#ENV RELX_REPLACE_OS_VARS=true
#
#WORKDIR /app
#
#COPY --from=builder /app/_build/default/rel/module9 ./module9
#
#CMD ["/app/module9/bin/module9", "foreground"]

#FROM erlang:27
#WORKDIR /app
#COPY . .
#ENV RELX_REPLACE_OS_VARS=true
#RUN rebar3 release
#
#CMD ["/app/_build/default/rel/module9/bin/module9", "foreground"]

FROM erlang:27

WORKDIR /app

# Copy project
COPY . .

# Build OTP release
ENV RELX_REPLACE_OS_VARS=true
RUN rebar3 release

#########################################
# ENTRYPOINT: configure erlang.cookie
#########################################
# Why env instead of baked password?
# → Because RELEASE_COOKIE is passed externally per environment

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

CMD ["/entrypoint.sh"]
