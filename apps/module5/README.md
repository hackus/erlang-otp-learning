# Starte docker with erlang in WSL
docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash

# Compile
erlc Concurency.erl.

# Run
Pid = 'Concurency':start().
Pid ! {msg, "hello"}.
Pid ! {msg, "world"}.
Pid ! get_count.
Pid ! stop.

# To exit
q().
or 
Ctrl + C