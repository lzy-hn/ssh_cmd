-module(ssh_cmd).
-author(yatung).

-define(SSH_TIMEOUT, 60000).

-export([run/4]).
-export([run/5]).

-export([cmd/4]).
-export([async_cmd/4, async_wait/1, controlling_process/2]).

run(Host, Port, Path, [Cmd0|_]=CmdList, Opts) when is_list(Cmd0) ->
    Cmd = lists:join(";", CmdList),
    FullCmd = lists:flatten(["cd ", Path, "; ", Cmd]),
    cmd(Host, Port, FullCmd, Opts);
run(Host, Port, Path, Cmd, Opts) ->
    FullCmd = lists:flatten(["cd ", Path, "; ", Cmd]),
    cmd(Host, Port, FullCmd, Opts).

run(Host, Port, [Cmd0|_]=CmdList, Opts) when is_list(Cmd0) ->
    Cmd = lists:join(";", CmdList),
    cmd(Host, Port, Cmd, Opts);
run(Host, Port, Cmd, Opts) ->
    cmd(Host, Port, Cmd, Opts).

cmd(Host, Port, Cmd, Opts0) ->
    Self = self(),
    Ref = erlang:make_ref(),
    Opts = [silently_accept_hosts|proplists:delete(silently_accept_hosts, Opts0)],
    Pid = spawn_link(fun () -> ssh_cmd(Host, Port, Cmd, Opts, Self, Ref) end),
    receive
        {ok, {Ref, Pid}, Data, Status} ->
            {ok, Data, Status};
        _ -> error(ssh)
    end.

async_cmd(Host, Port, Cmd, Opts) ->
    Self = self(),
    Ref = erlang:make_ref(),
    Pid = spawn_link(fun () -> ssh_cmd(Host, Port, Cmd, [async|Opts], Self, Ref) end),
    {Ref, Pid}.  %% ARef

controlling_process({_Ref, Pid}=ARef, NewOwner) ->
    Pid ! {controlling_process, self(), NewOwner},
    receive
        {ok, ARef, controlling_process} ->
            ok
    after ?SSH_TIMEOUT ->
        error({ssh, etimedout})
    end.

async_wait({Ref, Pid}=ARef) ->
    Pid ! {async_wait, Ref},
    receive
        {ok, ARef, Data, Status} ->
            {ok, Data, Status};
        _ -> error(ssh)
    after ?SSH_TIMEOUT ->
        error({ssh, etimedout})
    end.

ssh_cmd(Host, Port, Cmd, Opts, Owner, Ref) ->
    SshOpts = proplists:delete(async, Opts),
    case ssh:connect(Host, Port, SshOpts) of
        {ok, CRef}  ->
            ssh_cmd(CRef, Cmd, Opts, Owner, Ref);
        {error, _}=Err ->
            error({ssh, Err})
    end.

ssh_cmd(CRef, Cmd, Opts, Owner, Ref) ->
    try ssh_connection:session_channel(CRef, ?SSH_TIMEOUT) of
        {ok, Channel} ->
            ssh_exec(CRef, Channel, Cmd, Opts, Owner, Ref);
        Err ->
            error({ssh, Err})
    after
        ssh:close(CRef)
    end.

ssh_exec(CRef, Channel, Cmd, Opts, Owner, Ref) ->
    case ssh_connection:exec(CRef, Channel, Cmd, ?SSH_TIMEOUT) of
        success ->
            ssh_loop([{owner, Owner}, {ref, Ref}|Opts]);
        Error ->
            error({ssh, Error})
    end.

ssh_loop(Env) ->
    receive
        {ssh_cm, _Pid, {data, _, _, Data}} ->
            ssh_loop([{data, Data}|Env]);
        {ssh_cm, _Pid, {exit_status, _, Status}} ->
            ssh_loop([{exit_status, Status}|Env]);
        {ssh_cm, _Pid, {eof, _}} ->
            ssh_loop(Env);
        {ssh_cm, _Pid, {closed, _}} ->
            ssh_ret(Env)
    after ?SSH_TIMEOUT ->
        error({ssh, etimedout})
    end.

ssh_ret(Env) ->
    Async = proplists:get_bool(async, Env),
    if
        Async ->
            ssh_async_ret(Env);
        true ->
            ssh_sync_ret(Env)
    end.

ssh_sync_ret(Env) ->
    ssh_ret_s(Env).

ssh_ret_s(Env) ->
    Ret = proplists:get_value(data, Env),
    Status = proplists:get_value(exit_status, Env),
    Ref = proplists:get_value(ref, Env),
    Owner = proplists:get_value(owner, Env),
    Owner ! {ok, {Ref, self()}, Ret, Status}.

ssh_async_ret(Env) ->
    Ref = proplists:get_value(ref, Env),
    Owner = proplists:get_value(owner, Env),
    receive
        {controlling_process, Owner, NewOwner} ->
            Owner ! {ok, {Ref, self()}, controlling_process},
            ssh_async_ret([{owner, NewOwner}|Env]);
        {async_wait, Ref} ->
            ssh_ret_s(Env)
    after ?SSH_TIMEOUT ->
        error({ssh, etimedout})
    end.