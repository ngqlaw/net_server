-module(net_ct_hook).

%% API
-export([
    init/2,
    terminate/1
]).

init(Id, _Opts) ->
    {ok, _} = application:ensure_all_started(net_server),
    {ok, Id}.

terminate(_) ->
    ok = application:stop(net_server),
    ok.
