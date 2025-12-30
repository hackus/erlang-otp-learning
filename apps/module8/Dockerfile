FROM erlang:27
WORKDIR /app
COPY . .
ENV RELX_REPLACE_OS_VARS=true
RUN rebar3 release
CMD ["/app/_build/default/rel/module8/bin/module8", "foreground"]

