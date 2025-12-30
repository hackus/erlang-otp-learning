# Start docker with erlang in WSL
# In WSL
docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash

# Compile
erlc ./src/<File.erl>

# Run
erl

# To exit
exit(whereis('<File>'), kill).
or
unregister('<File>').
or
q().
or 
Ctrl + C