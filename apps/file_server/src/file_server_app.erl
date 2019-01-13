%%%-------------------------------------------------------------------
%% @doc file_server public API
%% @end
%%%-------------------------------------------------------------------

-module(file_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APP, file_server).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case file_server_sup:start_link() of
        {ok, Pid} ->
            post_start(),
            {ok, Pid};
        Res ->
            Res
    end.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

post_start() ->
    {ok, Port} = application:get_env(?APP, port),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, ?APP, "index.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, ?APP, ""}},
            {"/upload", upload_h, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    ok.