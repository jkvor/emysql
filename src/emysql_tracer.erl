-module(emysql_tracer).
-export([trace_module/1]).
-compile(export_all).

trace_module(Fun) ->
    spawn(fun() -> trace_module1(Fun) end).

trace_module1(Fun) ->
    Modules =
        [emysql] ++
        [emysql_auth] ++
        [emysql_conn] ++
        [emysql_conn_mgr] ++
        [emysql_statements] ++
        %%[emysql_tcp] ++
        [],
    [erlang:trace_pattern({Mod, '_', '_'},[{'_', [], [{return_trace}]}],[local]) || Mod <- Modules],
    S = self(),
    Pid = spawn(fun() -> do_trace(S, Fun) end),
    erlang:trace(all, true, [call,procs,exiting]),
    Pid ! {self(), start},
    trace_loop().

do_trace(Parent, Fun) ->
    receive
        {Parent, start} ->
            Fun()
    end.

trace_loop() ->
    receive
        %{trace, _, call, X} ->
        %   io:format("Call: ~p~n", [X]),
        %   trace_loop();
        %{trace, _, return_from, Call, Ret} ->
        %   io:format("Return From: ~p => ~p~n", [Call, Ret]),
        %   trace_loop();
        %{trace, _, send_to_non_existing_process, Msg, To} ->
        %   io:format("send_to_non_existing_process ~p to ~p~n", [Msg, To]),
        %   trace_loop();
        _Other ->
            io:format("~p~n", [_Other]),
            trace_loop()
    end.
