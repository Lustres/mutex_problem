%%%-------------------------------------------------------------------
%% @doc mutex_problem public API
%% @end
%%%-------------------------------------------------------------------

-module(mutex_problem_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Ret = mutex_problem_sup:start_link(),
    ProcessCount = application:get_env(process_count),
    lists:foreach(
        fun(E)->
            supervisor:start_child(mp_processes_sup, [E])
        end,
        lists:seq(1, ProcessCount)),
    Ret.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
