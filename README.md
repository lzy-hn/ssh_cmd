ssh_cmd
=====

通过erlang执行远程服务器命令

Build
-----

    $ rebar3 compile

Example
-------

    > ssh:start()
    > ssh_cmd:run("10.0.111.215", 22, "/tmp", "pwd", [{user, "yatung"}, {password, "123456"}]).
    
    {ok,<<"/tmp\n">>,0}
