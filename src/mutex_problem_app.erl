%%%-------------------------------------------------------------------
%% @doc mutex_problem public API
%% @end
%%%-------------------------------------------------------------------

-module(mutex_problem_app).

-behaviour(application).

%% API
-export([require/1, release/1]).

%% Application callbacks
-export([start/2, stop/1]).

-define(SERVER_ID(ID), list_to_atom(io_lib:format("P~p", [ID]))).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
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
-spec(require(Pi :: pos_integer()) -> ok | not_found).
require(Pi) when is_integer(Pi) ->
    {ok, ProcessCount} = application:get_env(process_count),
    if
        0 < Pi andalso Pi =< ProcessCount -> gen_server:cast(?SERVER_ID(Pi), require);
        true -> not_found
    end.

%%--------------------------------------------------------------------
%% @doc
%% release resource
%%
%% @end
%%--------------------------------------------------------------------
-spec(release(Pi :: pos_integer()) -> ok | not_found).
release(Pi) when is_integer(Pi) ->
    {ok, ProcessCount} = application:get_env(process_count),
    if
        0 < Pi andalso Pi =< ProcessCount -> gen_server:cast(?SERVER_ID(Pi), release);
        true -> not_found
    end.


%%====================================================================
%% Internal functions
%%====================================================================
