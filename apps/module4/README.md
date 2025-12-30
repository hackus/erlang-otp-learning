# Start docker with erlang in WSL
docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash

# Compile
erlc MyModule.erl

# Run
erl -pa . -s MyModule hello

# To exit
q().
or 
Ctrl + C