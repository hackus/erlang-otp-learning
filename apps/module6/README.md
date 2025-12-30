# Start docker with erlang in WSL
# In WSL
docker run -it --rm -v $(pwd):/app -w /app erlang:27 bash

# Compile
erlc ./src/RunListServer.erl

# Run
erl
10> 'RunListServer':start_link().
{ok,<0.102.0>}
11> 'RunListServer':get_sync().
[]
12> 'RunListServer':add_sync(10).
ok
13> 'RunListServer':add_sync(20).
ok
14> 'RunListServer':add_sync(30).
ok
15> 'RunListServer':get_sync().
[10,20,30]
16> 'RunListServer':add_async(40).
ok
17> 'RunListServer':add_async(50).
ok
18> 'RunListServer':add_async(60).
ok
19> 'RunListServer':get_sync().
[10,20,30,40,50,60]
20> 'RunListServer':clear_async().
ok
21> 'RunListServer':get_sync().
[]
22> 'RunListServer':add_async(60).
ok
23> 'RunListServer':get_sync().
"<"
24> 'RunListServer':add_async(70).
ok
25> 'RunListServer':get_sync().
"<F"
26> 'RunListServer':add_sync(10).
ok
27> 'RunListServer':get_sync().
"<F\n"
28> 'RunListServer':clear_sync().
ok
29> 'RunListServer':get_sync().
[]
30> 'RunListServer':add_sync(10).
ok
31> 'RunListServer':get_sync().
"\n"
32>

# To exit
exit(whereis('RunListServer'), kill).
or
unregister('RunListServer').
or
q().
or 
Ctrl + C