FROM erlang:27

WORKDIR /app

COPY rebar.config /app/

# Copy entire umbrella apps
COPY apps /app/apps

RUN rebar3 get-deps

CMD ["sh", "-c", "rebar3 compile && rebar3 eunit"]


#FROM erlang:27
#
#WORKDIR /app
#
## Copy project into the image
#COPY rebar.config /app/
#COPY src /app/src
#COPY test /app/test
#
## Compile during build
#RUN rebar3 compile
#
## Run tests automatically on container start
#CMD ["rebar3", "eunit"]
