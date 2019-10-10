%%%-------------------------------------------------------------------
%% @doc ssh_cmd public API
%% @end
%%%-------------------------------------------------------------------

-module(ssh_cmd_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ssh_cmd_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
