FROM erlang:27

WORKDIR /app

COPY . .

RUN rebar3 compile

EXPOSE 8080

CMD ["sh","-c", "erl -pa /app/_build/default/lib/*/ebin -eval 'application:ensure_all_started(module11_1).' -noshell"]