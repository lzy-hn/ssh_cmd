ssh_cmd
=====

通过erlang执行远程服务器命令

Build
-----

    $ rebar3 compile

Example
-------

    erl -pa _build/default/lib/ssh_cmd/ebin/
    
    1> application:ensure_all_started(ssh_cmd).
    {ok,[crypto,asn1,public_key,ssh,ssh_cmd]}

    2> ssh_cmd:run("10.0.111.215", 22, "/tmp", "pwd", [{user, "yatung"}, {password, "123456"}]).
    
    {ok,<<"/tmp\n">>,0}


    3> ARef = ssh_cmd:async_cmd("10.0.111.215", 22, "pwd", [{user, "yatung"}, {password, "123456"}]).
    
    {#Ref<0.1410333925.2679111681.233383>,<0.469.0>}

    4> do something ....
    5> ssh_cmd:async_wait(ARef).
    
    {ok,<<"/home/yatung\n">>,0}

