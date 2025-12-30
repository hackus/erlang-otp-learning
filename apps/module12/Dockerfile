FROM erlang:27

WORKDIR /app
COPY . /app

RUN rebar3 as prod release

RUN chmod +x /app/entrypoint.sh

ENTRYPOINT ["/app/entrypoint.sh"]
