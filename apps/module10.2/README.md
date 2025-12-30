# Start docker with erlang in WSL
# In WSL
docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash

# Compile
erlc ./src/<File.erl>

# Run
docker compose down
docker compose build --no-cache
docker compose up -d

docker attach module10_2
.
in erlang console:
> application:start(module10_2).
> supervisor:which_children(module10_2_sup).

> ets_advanced_counter_store:incr(job1).
> ets_advanced_counter_store:incr(job1).
> ets_advanced_counter_store:bulk_put([{a,10},{b,20}]).
> ets_advanced_counter_store:select([{{'$1','$2'},[],['$_']}]).
> exit(whereis(ets_advanced_counter_store), kill).
> whereis(ets_advanced_counter_store).
> ets_advanced_counter_store:select([{{'$1','$2'},[],['$_']}]).

Every time you see:
{undef, [{some_module,start_link,[],[]}, ...]}

your checklist is:
code:which(some_module). → is it compiled & on path?
some_module:module_info(exports). → does it export start_link/0?
Does the supervisor child spec call the same {Module, start_link, []}?

# To exit
exit(whereis('<File>'), kill).
or
unregister('<File>').
or
q().
or 
Ctrl + C