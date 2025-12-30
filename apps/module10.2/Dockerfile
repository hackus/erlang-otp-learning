FROM erlang:27

WORKDIR /app

# Copy project
COPY . .

# Build OTP release
#ENV RELX_REPLACE_OS_VARS=true
RUN rebar3 compile

CMD ["erl", "-pa", "/app/_build/default/lib/module10_2/ebin"]