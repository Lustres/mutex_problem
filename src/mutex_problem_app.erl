%%%-------------------------------------------------------------------
%% @doc mutex_problem public API
%% @end
%%%-------------------------------------------------------------------

-module(mutex_problem_app).

-behaviour(application).

%% API
-export([require/2]).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok = application:ensure_started(sasl),
    Ret = mutex_problem_sup:start_link(),
    {ok, ProcessCount} = application:get_env(process_count),
    lists:foreach(
        fun(E)->
            supervisor:start_child(mp_processes_sup, [E])
        end,
        lists:seq(1, ProcessCount)),
    Ret.

%%--------------------------------------------------------------------

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% make requirement to resource
%%
%% @end
%%--------------------------------------------------------------------
-spec(require(Pi :: pos_integer(), Duration :: non_neg_integer())
        -> {wait, ResOwner :: pos_integer() | undefined} | owned).
require(Pi, Duration) when is_integer(Pi) ->
    {ok, ProcessCount} = application:get_env(mutex_problem, process_count),
    if
        0 < Pi andalso Pi =< ProcessCount -> mp_process:require(Pi, Duration);
        true -> not_found
    end.
