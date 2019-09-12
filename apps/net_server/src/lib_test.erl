%% @doc 测试
-module(lib_test).

-behaviour(gen_server).

-export([
    test/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

test() ->
    {ok, Pid} = start_link([]),
    lager:info("main pro:~p", [Pid]),
    L = pro_ct(Pid, 10),
    timer:sleep(1000),
    lager:info("main pro msg len:~p", [process_info(Pid, message_queue_len)]),
    spawn(fun() -> exit(Pid, kill) end),
    timer:sleep(1000),
    case is_process_alive(Pid) of
        true ->
            lager:info("main pro alive:~p", [process_info(Pid, message_queue_len)]);
        false ->
            lager:info("main pro down:~p", [Pid])
    end,
    [begin
        case is_process_alive(P) of
            true ->
                lager:info("pro alive ~p", [process_info(P, message_queue_len)]);
            false ->
                lager:info("pro down:~p", [P])
        end
    end || P <- L],
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, ok};
init([Pid]) ->
    process_flag(trap_exit, true),
    {ok, Pid, 0}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_call(syn_call, _From, State) ->
    wait(3000),
    {reply, ok, State};
handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_info(timeout, State) ->
    case catch gen_server:call(State, syn_call) of
        Error ->
            lager:info("call error:~p", [Error])
    end,
    {noreply, State};
handle_info(Info, State) ->
    lager:info("other msg:~p", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:info("process ~p down:~p ~p", [self(), Reason, process_info(self(), message_queue_len)]),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

start(Args) ->
    gen_server:start(?MODULE, Args, []).

%% 启动多进程
pro_ct(Pid, N) ->
    [begin
        {ok, P} = start([Pid]),
        P
    end || _ <- lists:seq(1,N)].

wait(Time) ->
    receive
        ok -> ok
    after Time -> ok
    end.
